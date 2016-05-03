int initialize(void);
void step(void);
void shutdown(void);

void update_depth(void);
void update_image(void);
int update_user(void);
void update_skeleton(void);
void add_depth(unsigned short *arr, int n);
void add_image(unsigned char *arr, int n);
void add_user(unsigned short *arr, int n);
void add_skeleton(float *arr, int n);
