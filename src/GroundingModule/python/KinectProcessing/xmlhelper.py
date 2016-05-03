import xml.etree.ElementTree as xml
import numpy

def generate_skeleton_xml(frame, skeleton_element):
    head_element = xml.SubElement(skeleton_element,'head')
    head_2d_element = xml.SubElement(head_element,'point2d')
    xml.SubElement(head_2d_element,'x').text = str(frame['skel'][0]['HEAD']['2d'][0])
    xml.SubElement(head_2d_element,'y').text = str(frame['skel'][0]['HEAD']['2d'][1])

    head_3d_element = xml.SubElement(head_element,'point3d')
    xml.SubElement(head_3d_element,'x').text = str(frame['skel'][0]['HEAD']['3d'][0])
    xml.SubElement(head_3d_element,'y').text = str(frame['skel'][0]['HEAD']['3d'][1])
    xml.SubElement(head_3d_element,'z').text = str(frame['skel'][0]['HEAD']['3d'][2])
    
    neck_element = xml.SubElement(skeleton_element,'neck')
    neck_2d_element = xml.SubElement(neck_element,'point2d')
    xml.SubElement(neck_2d_element,'x').text = str(frame['skel'][0]['NECK']['2d'][0])
    xml.SubElement(neck_2d_element,'y').text = str(frame['skel'][0]['NECK']['2d'][1])

    neck_3d_element = xml.SubElement(neck_element,'point3d')
    xml.SubElement(neck_3d_element,'x').text = str(frame['skel'][0]['NECK']['3d'][0])
    xml.SubElement(neck_3d_element,'y').text = str(frame['skel'][0]['NECK']['3d'][1])
    xml.SubElement(neck_3d_element,'z').text = str(frame['skel'][0]['NECK']['3d'][2])

    right_shoulder_element = xml.SubElement(skeleton_element,'right_shoulder')
    right_shoulder_2d_element = xml.SubElement(right_shoulder_element,'point2d')
    xml.SubElement(right_shoulder_2d_element,'x').text = str(frame['skel'][0]['RIGHT_SHOULDER']['2d'][0])
    xml.SubElement(right_shoulder_2d_element,'y').text = str(frame['skel'][0]['RIGHT_SHOULDER']['2d'][1])

    right_shoulder_3d_element = xml.SubElement(right_shoulder_element,'point3d')
    xml.SubElement(right_shoulder_3d_element,'x').text = str(frame['skel'][0]['RIGHT_SHOULDER']['3d'][0])
    xml.SubElement(right_shoulder_3d_element,'y').text = str(frame['skel'][0]['RIGHT_SHOULDER']['3d'][1])
    xml.SubElement(right_shoulder_3d_element,'z').text = str(frame['skel'][0]['RIGHT_SHOULDER']['3d'][2])
    
    left_shoulder_element = xml.SubElement(skeleton_element,'left_shoulder')
    left_shoulder_2d_element = xml.SubElement(left_shoulder_element,'point2d')
    xml.SubElement(left_shoulder_2d_element,'x').text = str(frame['skel'][0]['LEFT_SHOULDER']['2d'][0])
    xml.SubElement(left_shoulder_2d_element,'y').text = str(frame['skel'][0]['LEFT_SHOULDER']['2d'][1])

    left_shoulder_3d_element = xml.SubElement(left_shoulder_element,'point3d')
    xml.SubElement(left_shoulder_3d_element,'x').text = str(frame['skel'][0]['LEFT_SHOULDER']['3d'][0])
    xml.SubElement(left_shoulder_3d_element,'y').text = str(frame['skel'][0]['LEFT_SHOULDER']['3d'][1])
    xml.SubElement(left_shoulder_3d_element,'z').text = str(frame['skel'][0]['LEFT_SHOULDER']['3d'][2])
    
    right_elbow_element = xml.SubElement(skeleton_element,'right_elbow')
    right_elbow_2d_element = xml.SubElement(right_elbow_element,'point2d')
    xml.SubElement(right_elbow_2d_element,'x').text = str(frame['skel'][0]['RIGHT_ELBOW']['2d'][0])
    xml.SubElement(right_elbow_2d_element,'y').text = str(frame['skel'][0]['RIGHT_ELBOW']['2d'][1])

    right_elbow_3d_element = xml.SubElement(right_elbow_element,'point3d')
    xml.SubElement(right_elbow_3d_element,'x').text = str(frame['skel'][0]['RIGHT_ELBOW']['3d'][0])
    xml.SubElement(right_elbow_3d_element,'y').text = str(frame['skel'][0]['RIGHT_ELBOW']['3d'][1])
    xml.SubElement(right_elbow_3d_element,'z').text = str(frame['skel'][0]['RIGHT_ELBOW']['3d'][2])
    
    left_elbow_element = xml.SubElement(skeleton_element,'left_elbow')
    left_elbow_2d_element = xml.SubElement(left_elbow_element,'point2d')
    xml.SubElement(left_elbow_2d_element,'x').text = str(frame['skel'][0]['LEFT_ELBOW']['2d'][0])
    xml.SubElement(left_elbow_2d_element,'y').text = str(frame['skel'][0]['LEFT_ELBOW']['2d'][1])

    left_elbow_3d_element = xml.SubElement(left_elbow_element,'point3d')
    xml.SubElement(left_elbow_3d_element,'x').text = str(frame['skel'][0]['LEFT_ELBOW']['3d'][0])
    xml.SubElement(left_elbow_3d_element,'z').text = str(frame['skel'][0]['LEFT_ELBOW']['3d'][2])
    xml.SubElement(left_elbow_3d_element,'y').text = str(frame['skel'][0]['LEFT_ELBOW']['3d'][1])
    
    right_hand_element = xml.SubElement(skeleton_element,'right_hand')
    right_hand_2d_element = xml.SubElement(right_hand_element,'point2d')
    xml.SubElement(right_hand_2d_element,'x').text = str(frame['skel'][0]['RIGHT_HAND']['2d'][0])
    xml.SubElement(right_hand_2d_element,'y').text = str(frame['skel'][0]['RIGHT_HAND']['2d'][1])

    right_hand_3d_element = xml.SubElement(right_hand_element,'point3d')
    xml.SubElement(right_hand_3d_element,'x').text = str(frame['skel'][0]['RIGHT_HAND']['3d'][0])
    xml.SubElement(right_hand_3d_element,'y').text = str(frame['skel'][0]['RIGHT_HAND']['3d'][1])
    xml.SubElement(right_hand_3d_element,'z').text = str(frame['skel'][0]['RIGHT_HAND']['3d'][2])
    
    left_hand_element = xml.SubElement(skeleton_element,'left_hand')
    left_hand_2d_element = xml.SubElement(left_hand_element,'point2d')
    xml.SubElement(left_hand_2d_element,'x').text = str(frame['skel'][0]['LEFT_HAND']['2d'][0])
    xml.SubElement(left_hand_2d_element,'y').text = str(frame['skel'][0]['LEFT_HAND']['2d'][1])

    left_hand_3d_element = xml.SubElement(left_hand_element,'point3d')
    xml.SubElement(left_hand_3d_element,'x').text = str(frame['skel'][0]['LEFT_HAND']['3d'][0])
    xml.SubElement(left_hand_3d_element,'y').text = str(frame['skel'][0]['LEFT_HAND']['3d'][1])
    xml.SubElement(left_hand_3d_element,'z').text = str(frame['skel'][0]['LEFT_HAND']['3d'][2])
    
    right_hip_element = xml.SubElement(skeleton_element,'right_hip')
    right_hip_2d_element = xml.SubElement(right_hip_element,'point2d')
    xml.SubElement(right_hip_2d_element,'x').text = str(frame['skel'][0]['RIGHT_HIP']['2d'][0])
    xml.SubElement(right_hip_2d_element,'y').text = str(frame['skel'][0]['RIGHT_HIP']['2d'][1])

    right_hip_3d_element = xml.SubElement(right_hip_element,'point3d')
    xml.SubElement(right_hip_3d_element,'x').text = str(frame['skel'][0]['RIGHT_HIP']['3d'][0])
    xml.SubElement(right_hip_3d_element,'y').text = str(frame['skel'][0]['RIGHT_HIP']['3d'][1])
    xml.SubElement(right_hip_3d_element,'z').text = str(frame['skel'][0]['RIGHT_HIP']['3d'][2])
    
    left_hip_element = xml.SubElement(skeleton_element,'left_hip')
    left_hip_2d_element = xml.SubElement(left_hip_element,'point2d')
    xml.SubElement(left_hip_2d_element,'x').text = str(frame['skel'][0]['LEFT_HIP']['2d'][0])
    xml.SubElement(left_hip_2d_element,'y').text = str(frame['skel'][0]['LEFT_HIP']['2d'][1])

    left_hip_3d_element = xml.SubElement(left_hip_element,'point3d')
    xml.SubElement(left_hip_3d_element,'x').text = str(frame['skel'][0]['LEFT_HIP']['3d'][0])
    xml.SubElement(left_hip_3d_element,'y').text = str(frame['skel'][0]['LEFT_HIP']['3d'][1])
    xml.SubElement(left_hip_3d_element,'z').text = str(frame['skel'][0]['LEFT_HIP']['3d'][2])
    
    right_knee_element = xml.SubElement(skeleton_element,'right_knee')
    right_knee_2d_element = xml.SubElement(right_knee_element,'point2d')
    xml.SubElement(right_knee_2d_element,'x').text = str(frame['skel'][0]['RIGHT_KNEE']['2d'][0])
    xml.SubElement(right_knee_2d_element,'y').text = str(frame['skel'][0]['RIGHT_KNEE']['2d'][1])

    right_knee_3d_element = xml.SubElement(right_knee_element,'point3d')
    xml.SubElement(right_knee_3d_element,'x').text = str(frame['skel'][0]['RIGHT_KNEE']['3d'][0])
    xml.SubElement(right_knee_3d_element,'y').text = str(frame['skel'][0]['RIGHT_KNEE']['3d'][1])
    xml.SubElement(right_knee_3d_element,'z').text = str(frame['skel'][0]['RIGHT_KNEE']['3d'][2])
    
    left_knee_element = xml.SubElement(skeleton_element,'left_knee')
    left_knee_2d_element = xml.SubElement(left_knee_element,'point2d')
    xml.SubElement(left_knee_2d_element,'x').text = str(frame['skel'][0]['LEFT_KNEE']['2d'][0])
    xml.SubElement(left_knee_2d_element,'y').text = str(frame['skel'][0]['LEFT_KNEE']['2d'][1])

    left_knee_3d_element = xml.SubElement(left_knee_element,'point3d')
    xml.SubElement(left_knee_3d_element,'x').text = str(frame['skel'][0]['LEFT_KNEE']['3d'][0])
    xml.SubElement(left_knee_3d_element,'y').text = str(frame['skel'][0]['LEFT_KNEE']['3d'][1])
    xml.SubElement(left_knee_3d_element,'z').text = str(frame['skel'][0]['LEFT_KNEE']['3d'][2])
    
    right_foot_element = xml.SubElement(skeleton_element,'right_foot')
    right_foot_2d_element = xml.SubElement(right_foot_element,'point2d')
    xml.SubElement(right_foot_2d_element,'x').text = str(frame['skel'][0]['RIGHT_FOOT']['2d'][0])
    xml.SubElement(right_foot_2d_element,'y').text = str(frame['skel'][0]['RIGHT_FOOT']['2d'][1])

    right_foot_3d_element = xml.SubElement(right_foot_element,'point3d')
    xml.SubElement(right_foot_3d_element,'x').text = str(frame['skel'][0]['RIGHT_FOOT']['3d'][0])
    xml.SubElement(right_foot_3d_element,'y').text = str(frame['skel'][0]['RIGHT_FOOT']['3d'][1])
    xml.SubElement(right_foot_3d_element,'z').text = str(frame['skel'][0]['RIGHT_FOOT']['3d'][2])
    
    left_foot_element = xml.SubElement(skeleton_element,'left_foot')
    left_foot_2d_element = xml.SubElement(left_foot_element,'point2d')
    xml.SubElement(left_foot_2d_element,'x').text = str(frame['skel'][0]['LEFT_FOOT']['2d'][0])
    xml.SubElement(left_foot_2d_element,'y').text = str(frame['skel'][0]['LEFT_FOOT']['2d'][1])

    left_foot_3d_element = xml.SubElement(left_foot_element,'point3d')
    xml.SubElement(left_foot_3d_element,'x').text = str(frame['skel'][0]['LEFT_FOOT']['3d'][0])
    xml.SubElement(left_foot_3d_element,'y').text = str(frame['skel'][0]['LEFT_FOOT']['3d'][1])
    xml.SubElement(left_foot_3d_element,'z').text = str(frame['skel'][0]['LEFT_FOOT']['3d'][2])
