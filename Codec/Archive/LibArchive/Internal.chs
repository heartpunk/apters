{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codec.Archive.LibArchive.Internal where

import Control.Arrow
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import Data.Int
import Foreign
import Foreign.C

#include <archive.h>

{# fun pure unsafe archive_version_number as ^ {} -> `Int' fromIntegral #}
{# fun pure unsafe archive_version_string as ^ {} -> `String' peekCAString* #}

newtype Archive = Archive (Ptr Archive) deriving Storable
{# pointer *archive as Archive nocode #}
newtype ArchiveEntry = ArchiveEntry (Ptr ArchiveEntry) deriving Storable
{# pointer *archive_entry as ArchiveEntry nocode #}

#c
enum ArchiveErrno {
    ArchiveEOF = ARCHIVE_EOF,
    ArchiveOK = ARCHIVE_OK,
    ArchiveRetry = ARCHIVE_RETRY,
    ArchiveWarn = ARCHIVE_WARN,
    ArchiveFailed = ARCHIVE_FAILED,
    ArchiveFatal = ARCHIVE_FATAL
};
#endc
{# enum ArchiveErrno {} deriving (Eq) #}

integralToEnum :: (Integral i, Enum e) => i -> e
integralToEnum = toEnum . fromIntegral

enumToIntegral :: (Integral i, Enum e) => e -> i
enumToIntegral = fromIntegral . fromEnum

withMaker :: IO a -> (a -> IO b) -> IO b
withMaker = (>>=)

withUserPointer :: a -> (Ptr () -> IO b) -> IO b
withUserPointer = withMaker . liftM castStablePtrToPtr . newStablePtr

foreign import ccall "wrapper" makeReadCallback'_ :: {# type archive_read_callback #} -> IO (FunPtr {# type archive_read_callback #})

type ArchiveReadCallback a = Archive -> a -> IO (Ptr (), CLong)

withReadCallback :: ArchiveReadCallback a -> (FunPtr {# type archive_read_callback #} -> IO b) -> IO b
withReadCallback f = withMaker $ makeReadCallback'_ $ \ archive user result -> do
    user' <- deRefStablePtr $ castPtrToStablePtr user
    (buf, len) <- f archive user'
    poke result buf
    return len

foreign import ccall "wrapper" makeSkipCallback'_ :: {# type archive_skip_callback #} -> IO (FunPtr {# type archive_skip_callback #})

type ArchiveSkipCallback a = Archive -> a -> CLong -> IO CLong

withSkipCallback :: ArchiveSkipCallback a -> (FunPtr {# type archive_skip_callback #} -> IO b) -> IO b
withSkipCallback f = withMaker $ makeSkipCallback'_ $ \ archive user len -> do
    user' <- deRefStablePtr $ castPtrToStablePtr user
    f archive user' len

-- type ArchiveWriteCallback a = FunPtr (Archive -> Ptr a -> Ptr () -> CULong -> IO CLong)

foreign import ccall "wrapper" makeOpenCallback'_ :: {# type archive_open_callback #} -> IO (FunPtr {# type archive_open_callback #})

type ArchiveOpenCallback a = Archive -> a -> IO ArchiveErrno

withOpenCallback :: ArchiveOpenCallback a -> (FunPtr {# type archive_open_callback #} -> IO b) -> IO b
withOpenCallback f = withMaker $ makeOpenCallback'_ $ \ archive user -> do
    user' <- deRefStablePtr $ castPtrToStablePtr user
    liftM enumToIntegral $ f archive user'

foreign import ccall "wrapper" makeCloseCallback'_ :: {# type archive_close_callback #} -> IO (FunPtr {# type archive_close_callback #})

type ArchiveCloseCallback a = Archive -> a -> IO ArchiveErrno

withCloseCallback :: ArchiveCloseCallback a -> (FunPtr {# type archive_close_callback #} -> IO b) -> IO b
withCloseCallback f = withMaker $ makeCloseCallback'_ $ \ archive user -> do
    let stptr = castPtrToStablePtr user
    user' <- deRefStablePtr stptr
    freeStablePtr stptr
    liftM enumToIntegral $ f archive user'

#c
typedef void archive_progress_callback(void *);
#endc

foreign import ccall "wrapper" makeProgressCallback'_ :: {# type archive_progress_callback #} -> IO (FunPtr {# type archive_progress_callback #})

type ArchiveProgressCallback a = a -> IO ()

withProgressCallback :: ArchiveProgressCallback a -> (FunPtr {# type archive_progress_callback #} -> IO b) -> IO b
withProgressCallback f = withMaker $ makeProgressCallback'_ $ \ user -> do
    user' <- deRefStablePtr $ castPtrToStablePtr user
    f user'

#c
enum ArchiveCompression {
    ArchiveCompressionNone = ARCHIVE_COMPRESSION_NONE,
    ArchiveCompressionGzip = ARCHIVE_COMPRESSION_GZIP,
    ArchiveCompressionBzip2 = ARCHIVE_COMPRESSION_BZIP2,
    ArchiveCompressionCompress = ARCHIVE_COMPRESSION_COMPRESS,
    ArchiveCompressionProgram = ARCHIVE_COMPRESSION_PROGRAM,
    ArchiveCompressionLZMA = ARCHIVE_COMPRESSION_LZMA,
    ArchiveCompressionXZ = ARCHIVE_COMPRESSION_XZ,
    ArchiveCompressionUU = ARCHIVE_COMPRESSION_UU,
    ArchiveCompressionRPM = ARCHIVE_COMPRESSION_RPM
};
#endc
{# enum ArchiveCompression {} deriving (Eq) #}

#c
enum ArchiveFormat {
    ArchiveFormatCpio = ARCHIVE_FORMAT_CPIO,
    ArchiveFormatCpioPOSIX = ARCHIVE_FORMAT_CPIO_POSIX,
    ArchiveFormatCpioBinLE = ARCHIVE_FORMAT_CPIO_BIN_LE,
    ArchiveFormatCpioBinBE = ARCHIVE_FORMAT_CPIO_BIN_BE,
    ArchiveFormatCpioSVR4NoCRC = ARCHIVE_FORMAT_CPIO_SVR4_NOCRC,
    ArchiveFormatCpioSVR4CRC = ARCHIVE_FORMAT_CPIO_SVR4_CRC,
    ArchiveFormatShar = ARCHIVE_FORMAT_SHAR,
    ArchiveFormatSharBase = ARCHIVE_FORMAT_SHAR_BASE,
    ArchiveFormatSharDump = ARCHIVE_FORMAT_SHAR_DUMP,
    ArchiveFormatTar = ARCHIVE_FORMAT_TAR,
    ArchiveFormatTarUSTar = ARCHIVE_FORMAT_TAR_USTAR,
    ArchiveFormatTarPaxInterchange = ARCHIVE_FORMAT_TAR_PAX_INTERCHANGE,
    ArchiveFormatTarPaxRestricted = ARCHIVE_FORMAT_TAR_PAX_RESTRICTED,
    ArchiveFormatTarGnuTar = ARCHIVE_FORMAT_TAR_GNUTAR,
    ArchiveFormatISO9660 = ARCHIVE_FORMAT_ISO9660,
    ArchiveFormatISO9660Rockridge = ARCHIVE_FORMAT_ISO9660_ROCKRIDGE,
    ArchiveFormatZip = ARCHIVE_FORMAT_ZIP,
    ArchiveFormatEmpty = ARCHIVE_FORMAT_EMPTY,
    ArchiveFormatAr = ARCHIVE_FORMAT_AR,
    ArchiveFormatArGNU = ARCHIVE_FORMAT_AR_GNU,
    ArchiveFormatArBSD = ARCHIVE_FORMAT_AR_BSD,
    ArchiveFormatMTREE = ARCHIVE_FORMAT_MTREE,
    ArchiveFormatRaw = ARCHIVE_FORMAT_RAW,
    ArchiveFormatXAR = ARCHIVE_FORMAT_XAR
};
#endc
{# enum ArchiveFormat {} deriving (Eq) #}

{# fun unsafe archive_read_new as ^ {} -> `Archive' id #}

{# fun unsafe archive_read_support_compression_all as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_bzip2 as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_compress as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_gzip as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_lzma as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_none as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_program as ^ {id `Archive', withCAString* `String'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_program_signature as ^ {id `Archive', withCAString* `String', asVoidPtrLen* `B.ByteString'&} -> `ArchiveErrno' integralToEnum #}
    where asVoidPtrLen bs f = unsafeUseAsCStringLen bs (f . (castPtr *** fromIntegral))
{# fun unsafe archive_read_support_compression_rpm as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_uu as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_compression_xz as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}

{# fun unsafe archive_read_support_format_all as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_ar as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_cpio as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_empty as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_gnutar as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_iso9660 as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_mtree as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_raw as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_tar as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_xar as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_support_format_zip as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}

{# fun archive_read_open as ^ {id `Archive', withUserPointer* `a', withOpenCallback* `ArchiveOpenCallback a', withReadCallback* `ArchiveReadCallback a', withCloseCallback* `ArchiveCloseCallback a'} -> `ArchiveErrno' integralToEnum #}
{# fun archive_read_open2 as ^ {id `Archive', withUserPointer* `a', withOpenCallback* `ArchiveOpenCallback a', withReadCallback* `ArchiveReadCallback a', withSkipCallback* `ArchiveSkipCallback a', withCloseCallback* `ArchiveCloseCallback a'} -> `ArchiveErrno' integralToEnum #}

{# fun archive_read_next_header as ^ {id `Archive', alloca- `ArchiveEntry' peek*} -> `ArchiveErrno' integralToEnum #}
{# fun archive_read_next_header2 as ^ {id `Archive', id `ArchiveEntry'} -> `ArchiveErrno' integralToEnum #}

{# fun archive_read_header_position as ^ {id `Archive'} -> `Int64' fromIntegral #}

{# fun archive_read_data as ^ {id `Archive', id `Ptr ()', id `CULong'} -> `CLong' id #}
{# fun archive_read_data_block as ^ {id `Archive', alloca- `Ptr ()' peek*, alloca- `CULong' peek*, alloca- `CLong' peek*} -> `ArchiveErrno' integralToEnum #}

{# fun archive_read_data_skip as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}

{# fun archive_read_set_format_options as ^ {id `Archive', withCAString* `String'} -> `ArchiveErrno' integralToEnum #}
{# fun archive_read_set_filter_options as ^ {id `Archive', withCAString* `String'} -> `ArchiveErrno' integralToEnum #}
{# fun archive_read_set_options as ^ {id `Archive', withCAString* `String'} -> `ArchiveErrno' integralToEnum #}

#c
enum ArchiveExtract {
    ArchiveExtractOwner = ARCHIVE_EXTRACT_OWNER,
    ArchiveExtractPerm = ARCHIVE_EXTRACT_PERM,
    ArchiveExtractTime = ARCHIVE_EXTRACT_TIME,
    ArchiveExtractNoOverwrite = ARCHIVE_EXTRACT_NO_OVERWRITE,
    ArchiveExtractUnlink = ARCHIVE_EXTRACT_UNLINK,
    ArchiveExtractACL = ARCHIVE_EXTRACT_ACL,
    ArchiveExtractFFlags = ARCHIVE_EXTRACT_FFLAGS,
    ArchiveExtractXAttr = ARCHIVE_EXTRACT_XATTR,
    ArchiveExtractSecureSymlinks = ARCHIVE_EXTRACT_SECURE_SYMLINKS,
    ArchiveExtractSecureNoDotDot = ARCHIVE_EXTRACT_SECURE_NODOTDOT,
    ArchiveExtractNoAutodir = ARCHIVE_EXTRACT_NO_AUTODIR,
    ArchiveExtractNoOverwriteNewer = ARCHIVE_EXTRACT_NO_OVERWRITE_NEWER,
    ArchiveExtractSparse = ARCHIVE_EXTRACT_SPARSE
};
#endc
{# enum ArchiveExtract {} deriving (Eq) #}

{# fun archive_read_extract as ^ {id `Archive', id `ArchiveEntry', id `CInt'} -> `ArchiveErrno' integralToEnum #}
{# fun archive_read_extract2 as ^ {id `Archive', id `ArchiveEntry', id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_read_extract_set_progress_callback as ^ {id `Archive', withProgressCallback* `ArchiveProgressCallback a', withUserPointer* `a'} -> `()' #}

{# fun unsafe archive_read_extract_set_skip_file as ^ {id `Archive', id `CULong', id `CULong'} -> `()' #}

{# fun archive_read_close as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}
{# fun archive_read_finish as ^ {id `Archive'} -> `ArchiveErrno' integralToEnum #}

-- TODO: archive_write, including archive_read_disk

{# fun unsafe archive_position_compressed as ^ {id `Archive'} -> `Int64' fromIntegral #}
{# fun unsafe archive_position_uncompressed as ^ {id `Archive'} -> `Int64' fromIntegral #}
{# fun unsafe archive_compression_name as ^ {id `Archive'} -> `String' peekCAString* #}
{# fun unsafe archive_compression as ^ {id `Archive'} -> `ArchiveCompression' integralToEnum #}
{# fun unsafe archive_errno as ^ {id `Archive'} -> `Int' fromIntegral #}
{# fun unsafe archive_error_string as ^ {id `Archive'} -> `String' peekCAString* #}
{# fun unsafe archive_format_name as ^ {id `Archive'} -> `String' peekCAString* #}
{# fun unsafe archive_format as ^ {id `Archive'} -> `ArchiveFormat' integralToEnum #}
{# fun unsafe archive_clear_error as ^ {id `Archive'} -> `()' #}
-- TODO: archive_set_error ("Calling variadic functions is not supported by the FFI")
{# fun unsafe archive_copy_error as ^ {id `Archive', id `Archive'} -> `()' #}
{# fun unsafe archive_file_count as ^ {id `Archive'} -> `Int' fromIntegral #}
