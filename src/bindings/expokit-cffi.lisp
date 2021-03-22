;;;; Generated on 10/01/2017 at 18:20:51 (UTC-8).

(COMMON-LISP:DECLAIM
 (COMMON-LISP:OPTIMIZE (COMMON-LISP:SPEED 0) COMMON-LISP:SAFETY
  COMMON-LISP:DEBUG COMMON-LISP:COMPILATION-SPEED))

(COMMON-LISP:IN-PACKAGE #:MAGICL.EXPOKIT-CFFI)

(COMMON-LISP:DECLAIM
 (COMMON-LISP:INLINE %%DGPADM %DGPADM %%DSPADM %DSPADM %%ZGPADM %ZGPADM
  %%ZHPADM %ZHPADM %%DGCHBV %DGCHBV %%DNCHBV %DNCHBV %%DSCHBV %DSCHBV %%ZGCHBV
  %ZGCHBV %%ZNCHBV %ZNCHBV))

(CFFI:DEFCFUN ("dgpadm_" %%DGPADM :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (IDEG :POINTER)
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (WSP :POINTER)
  (LWSP :POINTER)
  (IPIV :POINTER)
  (IEXPH :POINTER)
  (NS :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %DGPADM (IDEG M T H LDH WSP LWSP IPIV IEXPH NS IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%DGPADM)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IDEG)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT)
                        WSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LWSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IPIV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IEXPH)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) NS)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((IDEG-REF23977 ':INT32) (M-REF23978 ':INT32)
                              (T-REF23979 ':DOUBLE) (LDH-REF23981 ':INT32)
                              (LWSP-REF23983 ':INT32) (IEXPH-REF23985 ':INT32)
                              (NS-REF23986 ':INT32) (IFLAG-REF23987 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF IDEG-REF23977 :INT32) IDEG)
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF23978 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF23979 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF23981 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF LWSP-REF23983 :INT32) LWSP)
    (COMMON-LISP:SETF (CFFI:MEM-REF IEXPH-REF23985 :INT32) IEXPH)
    (COMMON-LISP:SETF (CFFI:MEM-REF NS-REF23986 :INT32) NS)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF23987 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF23980 H) (WSP-REF23982 WSP) (IPIV-REF23984 IPIV))
     (%%DGPADM IDEG-REF23977 M-REF23978 T-REF23979 H-REF23980 LDH-REF23981
      WSP-REF23982 LWSP-REF23983 IPIV-REF23984 IEXPH-REF23985 NS-REF23986
      IFLAG-REF23987))))

(CFFI:DEFCFUN ("dspadm_" %%DSPADM :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (IDEG :POINTER)
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (WSP :POINTER)
  (LWSP :POINTER)
  (IPIV :POINTER)
  (IEXPH :POINTER)
  (NS :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %DSPADM (IDEG M T H LDH WSP LWSP IPIV IEXPH NS IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%DSPADM)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IDEG)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT)
                        WSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LWSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IPIV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IEXPH)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) NS)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((IDEG-REF23988 ':INT32) (M-REF23989 ':INT32)
                              (T-REF23990 ':DOUBLE) (LDH-REF23992 ':INT32)
                              (LWSP-REF23994 ':INT32) (IEXPH-REF23996 ':INT32)
                              (NS-REF23997 ':INT32) (IFLAG-REF23998 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF IDEG-REF23988 :INT32) IDEG)
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF23989 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF23990 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF23992 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF LWSP-REF23994 :INT32) LWSP)
    (COMMON-LISP:SETF (CFFI:MEM-REF IEXPH-REF23996 :INT32) IEXPH)
    (COMMON-LISP:SETF (CFFI:MEM-REF NS-REF23997 :INT32) NS)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF23998 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF23991 H) (WSP-REF23993 WSP) (IPIV-REF23995 IPIV))
     (%%DSPADM IDEG-REF23988 M-REF23989 T-REF23990 H-REF23991 LDH-REF23992
      WSP-REF23993 LWSP-REF23994 IPIV-REF23995 IEXPH-REF23996 NS-REF23997
      IFLAG-REF23998))))

(CFFI:DEFCFUN ("zgpadm_" %%ZGPADM :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (IDEG :POINTER)
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (WSP :POINTER)
  (LWSP :POINTER)
  (IPIV :POINTER)
  (IEXPH :POINTER)
  (NS :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %ZGPADM (IDEG M T H LDH WSP LWSP IPIV IEXPH NS IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%ZGPADM)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IDEG)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LWSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IPIV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IEXPH)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) NS)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((IDEG-REF23999 ':INT32) (M-REF24000 ':INT32)
                              (T-REF24001 ':DOUBLE) (LDH-REF24003 ':INT32)
                              (LWSP-REF24005 ':INT32) (IEXPH-REF24007 ':INT32)
                              (NS-REF24008 ':INT32) (IFLAG-REF24009 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF IDEG-REF23999 :INT32) IDEG)
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24000 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24001 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24003 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF LWSP-REF24005 :INT32) LWSP)
    (COMMON-LISP:SETF (CFFI:MEM-REF IEXPH-REF24007 :INT32) IEXPH)
    (COMMON-LISP:SETF (CFFI:MEM-REF NS-REF24008 :INT32) NS)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF24009 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24002 H) (WSP-REF24004 WSP) (IPIV-REF24006 IPIV))
     (%%ZGPADM IDEG-REF23999 M-REF24000 T-REF24001 H-REF24002 LDH-REF24003
      WSP-REF24004 LWSP-REF24005 IPIV-REF24006 IEXPH-REF24007 NS-REF24008
      IFLAG-REF24009))))

(CFFI:DEFCFUN ("zhpadm_" %%ZHPADM :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (IDEG :POINTER)
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (WSP :POINTER)
  (LWSP :POINTER)
  (IPIV :POINTER)
  (IEXPH :POINTER)
  (NS :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %ZHPADM (IDEG M T H LDH WSP LWSP IPIV IEXPH NS IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%ZHPADM)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IDEG)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LWSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IPIV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IEXPH)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) NS)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((IDEG-REF24010 ':INT32) (M-REF24011 ':INT32)
                              (T-REF24012 ':DOUBLE) (LDH-REF24014 ':INT32)
                              (LWSP-REF24016 ':INT32) (IEXPH-REF24018 ':INT32)
                              (NS-REF24019 ':INT32) (IFLAG-REF24020 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF IDEG-REF24010 :INT32) IDEG)
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24011 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24012 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24014 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF LWSP-REF24016 :INT32) LWSP)
    (COMMON-LISP:SETF (CFFI:MEM-REF IEXPH-REF24018 :INT32) IEXPH)
    (COMMON-LISP:SETF (CFFI:MEM-REF NS-REF24019 :INT32) NS)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF24020 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24013 H) (WSP-REF24015 WSP) (IPIV-REF24017 IPIV))
     (%%ZHPADM IDEG-REF24010 M-REF24011 T-REF24012 H-REF24013 LDH-REF24014
      WSP-REF24015 LWSP-REF24016 IPIV-REF24017 IEXPH-REF24018 NS-REF24019
      IFLAG-REF24020))))

(CFFI:DEFCFUN ("dgchbv_" %%DGCHBV :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (Y :POINTER)
  (WSP :POINTER)
  (IWSP :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %DGCHBV (M T H LDH Y WSP IWSP IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%DGCHBV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) Y)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IWSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF24021 ':INT32) (T-REF24022 ':DOUBLE)
                              (LDH-REF24024 ':INT32) (IFLAG-REF24028 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24021 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24022 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24024 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF24028 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24023 H) (Y-REF24025 Y) (WSP-REF24026 WSP) (IWSP-REF24027 IWSP))
     (%%DGCHBV M-REF24021 T-REF24022 H-REF24023 LDH-REF24024 Y-REF24025
      WSP-REF24026 IWSP-REF24027 IFLAG-REF24028))))

(CFFI:DEFCFUN ("dnchbv_" %%DNCHBV :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (Y :POINTER)
  (WSP :POINTER))

(COMMON-LISP:DEFUN %DNCHBV (M T H LDH Y WSP)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%DNCHBV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) Y)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP))
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF24029 ':INT32) (T-REF24030 ':DOUBLE)
                              (LDH-REF24032 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24029 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24030 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24032 :INT32) LDH)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24031 H) (Y-REF24033 Y) (WSP-REF24034 WSP))
     (%%DNCHBV M-REF24029 T-REF24030 H-REF24031 LDH-REF24032 Y-REF24033
      WSP-REF24034))))

(CFFI:DEFCFUN ("dschbv_" %%DSCHBV :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (Y :POINTER)
  (WSP :POINTER)
  (IWSP :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %DSCHBV (M T H LDH Y WSP IWSP IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%DSCHBV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY COMMON-LISP:DOUBLE-FLOAT) Y)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IWSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF24035 ':INT32) (T-REF24036 ':DOUBLE)
                              (LDH-REF24038 ':INT32) (IFLAG-REF24042 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24035 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24036 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24038 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF24042 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24037 H) (Y-REF24039 Y) (WSP-REF24040 WSP) (IWSP-REF24041 IWSP))
     (%%DSCHBV M-REF24035 T-REF24036 H-REF24037 LDH-REF24038 Y-REF24039
      WSP-REF24040 IWSP-REF24041 IFLAG-REF24042))))

(CFFI:DEFCFUN ("zgchbv_" %%ZGCHBV :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (Y :POINTER)
  (WSP :POINTER)
  (IWSP :POINTER)
  (IFLAG :POINTER))

(COMMON-LISP:DEFUN %ZGCHBV (M T H LDH Y WSP IWSP IFLAG)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%ZGCHBV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        Y)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY (COMMON-LISP:SIGNED-BYTE 32)
                         (COMMON-LISP:*))
                        IWSP)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) IFLAG))
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF24043 ':INT32) (T-REF24044 ':DOUBLE)
                              (LDH-REF24046 ':INT32) (IFLAG-REF24050 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24043 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24044 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24046 :INT32) LDH)
    (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF24050 :INT32) IFLAG)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24045 H) (Y-REF24047 Y) (WSP-REF24048 WSP) (IWSP-REF24049 IWSP))
     (%%ZGCHBV M-REF24043 T-REF24044 H-REF24045 LDH-REF24046 Y-REF24047
      WSP-REF24048 IWSP-REF24049 IFLAG-REF24050))))

(CFFI:DEFCFUN ("znchbv_" %%ZNCHBV :LIBRARY MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT)
    :VOID
  (M :POINTER)
  (T :POINTER)
  (H :POINTER)
  (LDH :POINTER)
  (Y :POINTER)
  (WSP :POINTER))

(COMMON-LISP:DEFUN %ZNCHBV (M T H LDH Y WSP)
  (COMMON-LISP:DECLARE (COMMON-LISP:INLINE %%ZNCHBV)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) M)
                       (COMMON-LISP:TYPE COMMON-LISP:DOUBLE-FLOAT T)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        H)
                       (COMMON-LISP:TYPE (COMMON-LISP:SIGNED-BYTE 32) LDH)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        Y)
                       (COMMON-LISP:TYPE
                        (COMMON-LISP:SIMPLE-ARRAY
                         (COMMON-LISP:COMPLEX COMMON-LISP:DOUBLE-FLOAT)
                         (COMMON-LISP:*))
                        WSP))
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF24051 ':INT32) (T-REF24052 ':DOUBLE)
                              (LDH-REF24054 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF24051 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF T-REF24052 :DOUBLE) T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF24054 :INT32) LDH)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS
     ((H-REF24053 H) (Y-REF24055 Y) (WSP-REF24056 WSP))
     (%%ZNCHBV M-REF24051 T-REF24052 H-REF24053 LDH-REF24054 Y-REF24055
      WSP-REF24056))))

(COMMON-LISP:DECLAIM
 (COMMON-LISP:NOTINLINE %%DGPADM %DGPADM %%DSPADM %DSPADM %%ZGPADM %ZGPADM
  %%ZHPADM %ZHPADM %%DGCHBV %DGCHBV %%DNCHBV %DNCHBV %%DSCHBV %DSCHBV %%ZGCHBV
  %ZGCHBV %%ZNCHBV %ZNCHBV))

(MAGICL.FOREIGN-LIBRARIES::TRACK-SYMBOLS
 'MAGICL.FOREIGN-LIBRARIES:LIBEXPOKIT
 '(("DGPADM" "dgpadm_" %%DGPADM %DGPADM)
   ("DSPADM" "dspadm_" %%DSPADM %DSPADM)
   ("ZGPADM" "zgpadm_" %%ZGPADM %ZGPADM)
   ("ZHPADM" "zhpadm_" %%ZHPADM %ZHPADM)
   ("DGCHBV" "dgchbv_" %%DGCHBV %DGCHBV)
   ("DNCHBV" "dnchbv_" %%DNCHBV %DNCHBV)
   ("DSCHBV" "dschbv_" %%DSCHBV %DSCHBV)
   ("ZGCHBV" "zgchbv_" %%ZGCHBV %ZGCHBV)
   ("ZNCHBV" "znchbv_" %%ZNCHBV %ZNCHBV)))

(COMMON-LISP:EXPORT
 '(%DGPADM %DSPADM %ZGPADM %ZHPADM %DGCHBV %DNCHBV %DSCHBV %ZGCHBV %ZNCHBV)
 '#:MAGICL.EXPOKIT-CFFI)

;;; End of file.
