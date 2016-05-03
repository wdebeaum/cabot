#include <vector>
#include <cstring>
#include <XnOpenNI.h>
#include <XnCodecIDs.h>
#include <XnCppWrapper.h>

xn::Context g_Context;
std::vector<xn::DepthGenerator*> g_DepthGenerators;
std::vector<xn::ImageGenerator*> g_ImageGenerators;
xn::UserGenerator g_UserGenerator;

int g_NumKinects = 0;
std::vector<unsigned short*> g_depths;
std::vector<unsigned char*> g_images;
std::vector<unsigned short*> g_users;
std::vector<float*> g_skeletons;
#define SKEL_PT_SIZE 8
#define SKEL_USER_SIZE 64

XnBool g_bNeedPose = FALSE;
XnChar g_strPose[20] = "";

#define MAX_NUM_USERS 15
XnUserID g_aUsers[MAX_NUM_USERS];

void XN_CALLBACK_TYPE
User_NewUser(xn::UserGenerator& generator,\
        XnUserID nId, void* pCookie)
{
    XnUInt32 epochTime = 0;
    xnOSGetEpochTime(&epochTime);
    printf("[NIUSR:NEW ] %d (%d)\n", epochTime, nId);
    // New user found
    if (g_bNeedPose) {
        g_UserGenerator.GetPoseDetectionCap().StartPoseDetection(\
                g_strPose, nId);
    }
    else {
        g_UserGenerator.GetSkeletonCap().RequestCalibration(nId, TRUE);
    }
}

void XN_CALLBACK_TYPE
User_LostUser(xn::UserGenerator& generator,\
        XnUserID nId, void* pCookie)
{
    XnUInt32 epochTime = 0;
    xnOSGetEpochTime(&epochTime);
    printf("[NIUSR:LOST] %d (%d)\n", epochTime, nId);
}

void XN_CALLBACK_TYPE
UserPose_PoseDetected(\
        xn::PoseDetectionCapability& capability, const XnChar* strPose,\
        XnUserID nId, void* pCookie)
{
    XnUInt32 epochTime = 0;
    xnOSGetEpochTime(&epochTime);
    printf("[NIUSR:POSE] %d Pose %s detected (%d)\n", epochTime, strPose, nId);
    g_UserGenerator.GetPoseDetectionCap().StopPoseDetection(nId);
    g_UserGenerator.GetSkeletonCap().RequestCalibration(nId, TRUE);
}

void XN_CALLBACK_TYPE
UserCalibration_CalibrationStart(xn::SkeletonCapability& capability,\
        XnUserID nId, void* pCookie)
{
    XnUInt32 epochTime = 0;
    xnOSGetEpochTime(&epochTime);
    printf("[NIUSR:CALI] %d Calibration started (%d)\n", epochTime, nId);
}

void XN_CALLBACK_TYPE
UserCalibration_CalibrationComplete(xn::SkeletonCapability& capability,\
        XnUserID nId, XnCalibrationStatus eStatus, void* pCookie)
{
    XnUInt32 epochTime = 0;
    xnOSGetEpochTime(&epochTime);
    if (eStatus == XN_CALIBRATION_STATUS_OK) {
        // Calibration succeeded
        printf("[NIUSR:CALI] %d Calibration complete, start tracking (%d)\n",\
                epochTime, nId);
        g_UserGenerator.GetSkeletonCap().StartTracking(nId);
    }
    else {
        // Calibration failed
        printf("[NIUSR:CALI] %d Calibration failed (%d)\n", epochTime, nId);
        if(eStatus==XN_CALIBRATION_STATUS_MANUAL_ABORT) {
            printf("Manual abort occured, stop attempting to calibrate!");
            return;
        }
        if (g_bNeedPose) {
            g_UserGenerator.GetPoseDetectionCap().StartPoseDetection(\
                    g_strPose, nId);
        }
        else {
            g_UserGenerator.GetSkeletonCap().RequestCalibration(nId, TRUE);
        }
    }
}

#define CHECK_RC(nRetVal, what)                                     \
    if (nRetVal != XN_STATUS_OK)                                    \
    {                                                               \
        printf("[NIMOD:CHK ] %s failed: %s\n", what, xnGetStatusString(nRetVal));\
        return 0;                                             \
    }

int
initialize(void)
{
    XnStatus nRetVal = XN_STATUS_OK;
    xn::EnumerationErrors errors;
    
    /* read config file */
    //nRetVal = g_Context.InitFromXmlFile(filename, &errors);
 
    /* let's make our own settings */
    nRetVal = g_Context.Init();
    CHECK_RC(nRetVal, "Initializing");

    xn::NodeInfoList deviceNodes;
    xn::NodeInfoList depthNodes;
    xn::NodeInfoList imageNodes;
    xn::NodeInfoList userNodes;

    nRetVal = g_Context.EnumerateProductionTrees(\
            XN_NODE_TYPE_DEVICE, NULL, deviceNodes);
    CHECK_RC(nRetVal, "Enumerating devices");

    for(xn::NodeInfoList::Iterator nodeIter = deviceNodes.Begin();\
            nodeIter != deviceNodes.End(); ++nodeIter) {
        g_NumKinects += 1;
    }
    printf("[NIMOD:INIT] %d Kinects Found\n", g_NumKinects);

    if(g_NumKinects == 0) {
        // no need to go any further
        return 0;
    }

    nRetVal = g_Context.EnumerateProductionTrees(\
            XN_NODE_TYPE_DEPTH, NULL, depthNodes, NULL);
    CHECK_RC(nRetVal, "Enumerating depth nodes");

    for(xn::NodeInfoList::Iterator nodeIter = depthNodes.Begin();\
            nodeIter != depthNodes.End(); ++nodeIter) {
        xn::NodeInfo info = *nodeIter;

        XnMapOutputMode mode;
        mode.nXRes = 640;
        mode.nYRes = 480;
        mode.nFPS = 30;

        nRetVal = g_Context.CreateProductionTree(info);
        CHECK_RC(nRetVal, "Creating depth production tree");

        xn::DepthGenerator *g_DepthGenerator = new xn::DepthGenerator();
        nRetVal = info.GetInstance(*g_DepthGenerator);
        CHECK_RC(nRetVal, "Getting depth instance");
        g_DepthGenerator->SetMapOutputMode(mode);
        g_DepthGenerator->StartGenerating();

        g_DepthGenerators.push_back(g_DepthGenerator);
    }

    nRetVal = g_Context.EnumerateProductionTrees(\
            XN_NODE_TYPE_IMAGE, NULL, imageNodes, NULL);
    CHECK_RC(nRetVal, "Enumerating image nodes");

    for(xn::NodeInfoList::Iterator nodeIter = imageNodes.Begin();\
            nodeIter != imageNodes.End(); ++nodeIter) {
        xn::NodeInfo info = *nodeIter;

        XnMapOutputMode mode;
        mode.nXRes = 640;
        mode.nYRes = 480;
        mode.nFPS = 30;

        nRetVal = g_Context.CreateProductionTree(info);
        CHECK_RC(nRetVal, "Creating image production tree");

        xn::ImageGenerator *g_ImageGenerator = new xn::ImageGenerator();
        nRetVal = info.GetInstance(*g_ImageGenerator);
        CHECK_RC(nRetVal, "Getting image instance");
        g_ImageGenerator->SetMapOutputMode(mode);
        g_ImageGenerator->StartGenerating();

        g_ImageGenerators.push_back(g_ImageGenerator);
    }

    // Just use one user generator and skeleton (current OpenNI limitations)
    // We assume at least one depth generator

    xn::Query *query = new xn::Query();
    query->AddNeededNode(g_DepthGenerators[0]->GetName());
    nRetVal = g_UserGenerator.Create(g_Context, query);
    CHECK_RC(nRetVal, "Create new user node");

    if(!g_UserGenerator.IsCapabilitySupported(XN_CAPABILITY_SKELETON)) {
        printf("[NIMOD:ERR ] User generator doesn't support skeleton\n");
        return 0;
    }

    XnCallbackHandle hUserCallbacks, hCalibrationStart,\
        hCalibrationComplete, hPoseDetected;

    nRetVal = g_UserGenerator.RegisterUserCallbacks(\
            User_NewUser, User_LostUser, NULL, hUserCallbacks);
    CHECK_RC(nRetVal, "Register to user callbacks");
    nRetVal = g_UserGenerator.GetSkeletonCap().RegisterToCalibrationStart(\
            UserCalibration_CalibrationStart, NULL, hCalibrationStart);
    CHECK_RC(nRetVal, "Register to calibration start");
    nRetVal = g_UserGenerator.GetSkeletonCap().RegisterToCalibrationComplete(\
            UserCalibration_CalibrationComplete, NULL, hCalibrationComplete);
    CHECK_RC(nRetVal, "Register to calibration complete");

    if (g_UserGenerator.GetSkeletonCap().NeedPoseForCalibration())
    {
        g_bNeedPose = TRUE;
        if (!g_UserGenerator.IsCapabilitySupported(\
                    XN_CAPABILITY_POSE_DETECTION))
        {
            printf("[NIMOD:ERR ] Pose required, but not supported\n");
            return 0;
        }
        nRetVal = g_UserGenerator.GetPoseDetectionCap().RegisterToPoseDetected(\
                UserPose_PoseDetected, NULL, hPoseDetected);
        CHECK_RC(nRetVal, "Register to Pose Detected");
        g_UserGenerator.GetSkeletonCap().GetCalibrationPose(g_strPose);
    }
    g_UserGenerator.GetSkeletonCap().SetSkeletonProfile(XN_SKEL_PROFILE_ALL);

    g_DepthGenerators[0]->GetAlternativeViewPointCap().SetViewPoint(*g_ImageGenerators[0]);

    nRetVal = g_Context.StartGeneratingAll();
    CHECK_RC(nRetVal, "Start Generating All");

    return g_NumKinects;
}

void
step(void)
{
    g_Context.WaitAndUpdateAll();
}

void
shutdown(void)
{
    g_Context.Release();
}

void
update_depth(void)
{
    if(g_DepthGenerators.size() == 0) return;
    for(int i = 0; i < g_NumKinects; i += 1) {
        xn::DepthMetaData depthMD;
        g_DepthGenerators[i]->GetMetaData(depthMD);
        const XnDepthPixel *p = depthMD.Data();
        unsigned short *g_depth = g_depths[i];
        memcpy(g_depth, (const void *)p, 480*640*sizeof(unsigned short));
    }
}

void
update_image(void)
{
    if(g_ImageGenerators.size() == 0) return;
    for(int i = 0; i < g_NumKinects; i += 1) {
        xn::ImageMetaData imageMD;
        g_ImageGenerators[i]->GetMetaData(imageMD);
        const XnUInt8 *p = imageMD.Data();
        unsigned char *g_image = g_images[i];
        memcpy(g_image, (const void *)p, 480*640*3*sizeof(unsigned char));
    }
}

void
set_skel(int userIdx, int partIdx, float x, float y, float z, float i, float j, float conf)
{
    int idx = (userIdx*SKEL_USER_SIZE*SKEL_PT_SIZE)+(partIdx*SKEL_PT_SIZE);
    float *g_skel = g_skeletons[0];
    g_skel[idx++] = i;
    g_skel[idx++] = j;
    g_skel[idx++] = x;
    g_skel[idx++] = y;
    g_skel[idx++] = z;
    g_skel[idx++] = conf;
}

int
update_user(void)
{
    if(g_DepthGenerators.size() == 0) return 0;
    xn::SceneMetaData sceneMD;
    g_UserGenerator.GetUserPixels(0, sceneMD);
    const XnLabel *p = sceneMD.Data();
    unsigned short *g_user = g_users[0];
    memcpy(g_user, (const void *)p, 480*640*sizeof(unsigned short));

    XnUInt16 nUsers = MAX_NUM_USERS;
    g_UserGenerator.GetUsers(g_aUsers, nUsers);

    for(int k = 0; k < nUsers; k++) {
        XnPoint3D com;
        float x, y, z, i, j;
        g_UserGenerator.GetCoM(g_aUsers[k], com);
        x = com.X;
        y = com.Y;
        z = com.Z;
        XnPoint3D pt;
        g_DepthGenerators[0]->ConvertRealWorldToProjective(1, &com, &pt);
        i = pt.X;
        j = pt.Y;
        set_skel(k, 0, x, y, z, i, j, 1.);
    }

    return (int)nUsers;
}

void
get_joint(int userid, XnSkeletonJoint joint,\
          float *x, float *y, float *z, float *i, float *j, float *conf)
{
    XnSkeletonJointPosition jointpos;
    g_UserGenerator.GetSkeletonCap().GetSkeletonJointPosition(\
            g_aUsers[userid], joint, jointpos);
    *conf = jointpos.fConfidence;

    XnPoint3D pt = jointpos.position;
    *x = pt.X;
    *y = pt.Y;
    *z = pt.Z;
    g_DepthGenerators[0]->ConvertRealWorldToProjective(1, &pt, &pt);
    *i = pt.X;
    *j = pt.Y;
}

void
update_skeleton(void)
{
    XnUInt16 nUsers = MAX_NUM_USERS;
    g_UserGenerator.GetUsers(g_aUsers, nUsers);

    for(int k = 0; k < nUsers; k++) {
        if(g_UserGenerator.GetSkeletonCap().IsTracking(g_aUsers[k])) {
            float x, y, z, i, j, c;
            get_joint(k, XN_SKEL_HEAD, &x, &y, &z, &i, &j, &c);
            set_skel(k, 1, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_NECK, &x, &y, &z, &i, &j, &c);
            set_skel(k, 2, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_LEFT_SHOULDER, &x, &y, &z, &i, &j, &c);
            set_skel(k, 3, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_RIGHT_SHOULDER, &x, &y, &z, &i, &j, &c);
            set_skel(k, 4, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_LEFT_ELBOW, &x, &y, &z, &i, &j, &c);
            set_skel(k, 5, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_RIGHT_ELBOW, &x, &y, &z, &i, &j, &c);
            set_skel(k, 6, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_LEFT_HAND, &x, &y, &z, &i, &j, &c);
            set_skel(k, 7, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_RIGHT_HAND, &x, &y, &z, &i, &j, &c);
            set_skel(k, 8, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_TORSO, &x, &y, &z, &i, &j, &c);
            set_skel(k, 9, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_LEFT_HIP, &x, &y, &z, &i, &j, &c);
            set_skel(k, 10, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_RIGHT_HIP, &x, &y, &z, &i, &j, &c);
            set_skel(k, 11, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_LEFT_KNEE, &x, &y, &z, &i, &j, &c);
            set_skel(k, 12, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_RIGHT_KNEE, &x, &y, &z, &i, &j, &c);
            set_skel(k, 13, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_LEFT_FOOT, &x, &y, &z, &i, &j, &c);
            set_skel(k, 14, x, y, z, i, j, c);
            get_joint(k, XN_SKEL_RIGHT_FOOT, &x, &y, &z, &i, &j, &c);
            set_skel(k, 15, x, y, z, i, j, c);
        }
    }
}

void
add_depth(unsigned short *arr, int n)
{
    g_depths.push_back(arr);
}

void
add_image(unsigned char *arr, int n)
{
    g_images.push_back(arr);
}

void
add_user(unsigned short *arr, int n)
{
    g_users.push_back(arr);
}

void
add_skeleton(float *arr, int n)
{
    g_skeletons.push_back(arr);
}
