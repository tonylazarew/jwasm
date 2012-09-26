/****************************************************************************
*
*                            Open Watcom Project
*
*    Portions Copyright (c) 1983-2002 Sybase, Inc. All Rights Reserved.
*
*  ========================================================================
*
*    This file contains Original Code and/or Modifications of Original
*    Code as defined in and that are subject to the Sybase Open Watcom
*    Public License version 1.0 (the 'License'). You may not use this file
*    except in compliance with the License. BY USING THIS FILE YOU AGREE TO
*    ALL TERMS AND CONDITIONS OF THE LICENSE. A copy of the License is
*    provided with the Original Code and Modifications, and is also
*    available at www.sybase.com/developer/opensource.
*
*    The Original Code and all software distributed under the License are
*    distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
*    EXPRESS OR IMPLIED, AND SYBASE AND ALL CONTRIBUTORS HEREBY DISCLAIM
*    ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
*    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
*    NON-INFRINGEMENT. Please see the License for the specific language
*    governing rights and limitations under the License.
*
*  ========================================================================
*
* Description:  uint and int type declarations.
*               This file is included by globals.h
*
****************************************************************************/

#ifndef _INTTYPE_H_INCLUDED_
#define _INTTYPE_H_INCLUDED_

typedef unsigned        uint;

typedef unsigned char   uint_8;
typedef signed char     int_8;
typedef unsigned short  uint_16;
typedef signed short    int_16;
#if defined (LONG_IS_64BITS)
typedef unsigned int    uint_32;
typedef   signed int    int_32;
typedef unsigned long   uint_64;
typedef   signed long   int_64;
#else
typedef unsigned long      uint_32;
typedef   signed long      int_32;
#if defined(LLONG_MAX) || defined(__GNUC__)
typedef unsigned long long uint_64;
typedef   signed long long int_64;
#else
typedef unsigned __int64  uint_64;
#ifdef __OCC__
typedef          __int64  int_64;
#else
typedef   signed __int64  int_64;
#endif
#endif
#endif

#endif
