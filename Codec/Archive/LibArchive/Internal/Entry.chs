{-# LANGUAGE ForeignFunctionInterface #-}
module Codec.Archive.LibArchive.Internal.Entry where

{# import Codec.Archive.LibArchive.Internal #}
import Foreign
import Foreign.C

#include <archive_entry.h>

#c
enum ArchiveEntryType {
    ArchiveEntryTypeReg = AE_IFREG,
    ArchiveEntryTypeLink = AE_IFLNK,
    ArchiveEntryTypeSocket = AE_IFSOCK,
    ArchiveEntryTypeChar = AE_IFCHR,
    ArchiveEntryTypeBlock = AE_IFBLK,
    ArchiveEntryTypeDir = AE_IFDIR,
    ArchiveEntryTypeFIFO = AE_IFIFO
};
#endc
{# enum ArchiveEntryType {} deriving (Eq) #}

{# fun unsafe archive_entry_clear as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_clone as ^ {id `ArchiveEntry'} -> `ArchiveEntry' id #}
{# fun unsafe archive_entry_free as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_new as ^ {} -> `ArchiveEntry' id #}

{# fun unsafe archive_entry_atime as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_atime_nsec as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_atime_is_set as ^ {id `ArchiveEntry'} -> `Bool' toBool #}
{# fun unsafe archive_entry_birthtime as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_birthtime_nsec as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_birthtime_is_set as ^ {id `ArchiveEntry'} -> `Bool' toBool #}
{# fun unsafe archive_entry_ctime as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_ctime_nsec as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_ctime_is_set as ^ {id `ArchiveEntry'} -> `Bool' toBool #}
{# fun unsafe archive_entry_dev as ^ {id `ArchiveEntry'} -> `CULong' id #}
{# fun unsafe archive_entry_devmajor as ^ {id `ArchiveEntry'} -> `CULong' id #}
{# fun unsafe archive_entry_devminor as ^ {id `ArchiveEntry'} -> `CULong' id #}
{# fun unsafe archive_entry_filetype as ^ {id `ArchiveEntry'} -> `ArchiveEntryType' integralToEnum #}
{# fun unsafe archive_entry_fflags as ^ {id `ArchiveEntry', alloca- `CULong' peek*, alloca- `CULong' peek*} -> `()' #}
-- TODO: archive_entry_fflags_text is missing wide-char variant?
{# fun unsafe archive_entry_gid as ^ {id `ArchiveEntry'} -> `CUInt' id #}
{# fun unsafe archive_entry_gname_w as ^ {id `ArchiveEntry'} -> `String' wideString* #}
{# fun unsafe archive_entry_hardlink_w as ^ {id `ArchiveEntry'} -> `String' wideString* #}
{# fun unsafe archive_entry_ino as ^ {id `ArchiveEntry'} -> `CULong' id #}
{# fun unsafe archive_entry_ino64 as ^ {id `ArchiveEntry'} -> `Int64' fromIntegral #}
{# fun unsafe archive_entry_mode as ^ {id `ArchiveEntry'} -> `CUInt' id #}
{# fun unsafe archive_entry_mtime as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_mtime_nsec as ^ {id `ArchiveEntry'} -> `CLong' id #}
{# fun unsafe archive_entry_mtime_is_set as ^ {id `ArchiveEntry'} -> `Bool' toBool #}
{# fun unsafe archive_entry_nlink as ^ {id `ArchiveEntry'} -> `CUInt' id #}
{# fun unsafe archive_entry_pathname_w as ^ {id `ArchiveEntry'} -> `String' wideString* #}
{# fun unsafe archive_entry_rdev as ^ {id `ArchiveEntry'} -> `CULong' id #}
{# fun unsafe archive_entry_rdevmajor as ^ {id `ArchiveEntry'} -> `CULong' id #}
{# fun unsafe archive_entry_rdevminor as ^ {id `ArchiveEntry'} -> `CULong' id #}
-- TODO: archive_entry_sourcepath is missing wide-char variant?
{# fun unsafe archive_entry_size as ^ {id `ArchiveEntry'} -> `Int64' fromIntegral #}
{# fun unsafe archive_entry_size_is_set as ^ {id `ArchiveEntry'} -> `Bool' toBool #}
{# fun unsafe archive_entry_strmode as ^ {id `ArchiveEntry'} -> `String' peekCAString* #}
{# fun unsafe archive_entry_symlink_w as ^ {id `ArchiveEntry'} -> `String' wideString* #}
{# fun unsafe archive_entry_uid as ^ {id `ArchiveEntry'} -> `CUInt' id #}
{# fun unsafe archive_entry_uname_w as ^ {id `ArchiveEntry'} -> `String' wideString* #}

{# fun unsafe archive_entry_set_atime as ^ {id `ArchiveEntry', id `CLong', id `CLong'} -> `()' #}
{# fun unsafe archive_entry_unset_atime as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_set_birthtime as ^ {id `ArchiveEntry', id `CLong', id `CLong'} -> `()' #}
{# fun unsafe archive_entry_unset_birthtime as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_set_ctime as ^ {id `ArchiveEntry', id `CLong', id `CLong'} -> `()' #}
{# fun unsafe archive_entry_unset_ctime as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_set_dev as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_devmajor as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_devminor as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_filetype as ^ {id `ArchiveEntry', enumToIntegral `ArchiveEntryType'} -> `()' #}
{# fun unsafe archive_entry_set_fflags as ^ {id `ArchiveEntry', id `CULong', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_copy_fflags_text_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `Maybe String' maybeWideString* #}
{# fun unsafe archive_entry_set_gid as ^ {id `ArchiveEntry', id `CUInt'} -> `()' #}
{# fun unsafe archive_entry_copy_gname_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `()' #}
{# fun unsafe archive_entry_copy_hardlink_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `()' #}
{# fun unsafe archive_entry_set_ino as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_ino64 as ^ {id `ArchiveEntry', fromIntegral `Int64'} -> `()' #}
{# fun unsafe archive_entry_copy_link_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `()' #}
{# fun unsafe archive_entry_set_mode as ^ {id `ArchiveEntry', id `CUInt'} -> `()' #}
{# fun unsafe archive_entry_set_mtime as ^ {id `ArchiveEntry', id `CLong', id `CLong'} -> `()' #}
{# fun unsafe archive_entry_unset_mtime as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_set_nlink as ^ {id `ArchiveEntry', id `CUInt'} -> `()' #}
{# fun unsafe archive_entry_copy_pathname_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `()' #}
{# fun unsafe archive_entry_set_perm as ^ {id `ArchiveEntry', id `CUInt'} -> `()' #}
{# fun unsafe archive_entry_set_rdev as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_rdevmajor as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_rdevminor as ^ {id `ArchiveEntry', id `CULong'} -> `()' #}
{# fun unsafe archive_entry_set_size as ^ {id `ArchiveEntry', fromIntegral `Int64'} -> `()' #}
{# fun unsafe archive_entry_unset_size as ^ {id `ArchiveEntry'} -> `()' #}
-- TODO: archive_entry_copy_sourcepath is missing wide-char variant?
{# fun unsafe archive_entry_copy_symlink_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `()' #}
{# fun unsafe archive_entry_set_uid as ^ {id `ArchiveEntry', id `CUInt'} -> `()' #}
{# fun unsafe archive_entry_copy_uname_w as ^ {id `ArchiveEntry', withWideString* `String'} -> `()' #}

-- TODO: support archive_entry_stat and archive_entry_copy_stat?

#c
enum ArchiveEntryACL {
    ArchiveEntryACLExecute = ARCHIVE_ENTRY_ACL_EXECUTE,
    ArchiveEntryACLWrite = ARCHIVE_ENTRY_ACL_WRITE,
    ArchiveEntryACLRead = ARCHIVE_ENTRY_ACL_READ
};
#endc
{# enum ArchiveEntryACL {} deriving (Eq) #}

#c
enum ArchiveEntryACLType {
    ArchiveEntryACLTypeAccess = ARCHIVE_ENTRY_ACL_TYPE_ACCESS,
    ArchiveEntryACLTypeDefault = ARCHIVE_ENTRY_ACL_TYPE_DEFAULT
};
#endc
{# enum ArchiveEntryACLType {} deriving (Eq) #}

#c
enum ArchiveEntryACLTag {
    ArchiveEntryACLTagUser = ARCHIVE_ENTRY_ACL_USER,
    ArchiveEntryACLTagUserObj = ARCHIVE_ENTRY_ACL_USER_OBJ,
    ArchiveEntryACLTagGroup = ARCHIVE_ENTRY_ACL_GROUP,
    ArchiveEntryACLTagGroupObj = ARCHIVE_ENTRY_ACL_GROUP_OBJ,
    ArchiveEntryACLTagMask = ARCHIVE_ENTRY_ACL_MASK,
    ArchiveEntryACLTagOther = ARCHIVE_ENTRY_ACL_OTHER
};
#endc
{# enum ArchiveEntryACLTag {} deriving (Eq) #}

{# fun unsafe archive_entry_acl_clear as ^ {id `ArchiveEntry'} -> `()' #}
{# fun unsafe archive_entry_acl_add_entry_w as ^ {id `ArchiveEntry', enumToIntegral `ArchiveEntryACLType', id `CInt', enumToIntegral `ArchiveEntryACLTag', id `CInt', withWideString* `String'} -> `()' #}

{# fun unsafe archive_entry_acl_reset as ^ {id `ArchiveEntry', id `CInt'} -> `ArchiveErrno' integralToEnum #}
{# fun unsafe archive_entry_acl_next_w as ^ {id `ArchiveEntry', id `CInt', alloca- `ArchiveEntryACLType' peekEnum*, alloca- `CInt' peek*, alloca- `ArchiveEntryACLTag' peekEnum*, alloca- `CInt' peek*, alloca- `String' wideStringPtr*} -> `ArchiveErrno' integralToEnum #}

#c
enum ArchiveEntryACLStyle {
    ArchiveEntryACLStyleAccess = ARCHIVE_ENTRY_ACL_TYPE_ACCESS,
    ArchiveEntryACLStyleDefault = ARCHIVE_ENTRY_ACL_TYPE_DEFAULT,
    ArchiveEntryACLStyleExtraID = ARCHIVE_ENTRY_ACL_STYLE_EXTRA_ID,
    ArchiveEntryACLStyleMarkDefault = ARCHIVE_ENTRY_ACL_STYLE_MARK_DEFAULT
};
#endc
{# enum ArchiveEntryACLStyle {} deriving (Eq) #}

{# fun unsafe archive_entry_acl_text_w as ^ {id `ArchiveEntry', id `CInt'} -> `String' wideString* #}
{# fun unsafe archive_entry_acl_count as ^ {id `ArchiveEntry', enumToIntegral `ArchiveEntryACLType'} -> `CInt' id #}

-- TODO: implement archive_entry_xattr_* and linkresolver

enumToIntegral :: (Integral i, Enum e) => e -> i
enumToIntegral = fromIntegral . fromEnum

peekEnum :: (Storable i, Integral i, Enum e) => Ptr i -> IO e
peekEnum p = do
    i <- peek p
    return $! integralToEnum i

withWideString :: String -> (Ptr a -> IO b) -> IO b
withWideString s f = withCWString s (f . castPtr)

wideString :: Ptr a -> IO String
wideString = peekCWString . castPtr

wideStringPtr :: Ptr (Ptr a) -> IO String
wideStringPtr p = peek p >>= wideString

maybeWideString :: Ptr a -> IO (Maybe String)
maybeWideString = maybePeek wideString
