#ifndef PLATFORM_H
#define PLATFORM_H

#include "../thirdparty/cplus.h"

typedef enum : u8 {
	TP_NULL = 0,
	TP_LINUX,
	TP_WINDOWS,
	TP_MACOS,
} TargetPlatform;

#endif
