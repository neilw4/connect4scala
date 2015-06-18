LOCAL_PATH:= $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := C4Ai
LOCAL_CFLAGS     := -Werror -Wall
LOCAL_SRC_FILES := C4Ai.c

include $(BUILD_SHARED_LIBRARY)
