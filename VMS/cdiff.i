#-h-  cdiff                      1096  local   12/03/80  16:26:16
   sym_pointer _
      Old_count (MAX_FILE_SIZE),
      New_count (MAX_FILE_SIZE),
      Old_xref (MAX_FILE_SIZE),
      New_xref (MAX_FILE_SIZE),
      Old_lno (MAX_UNIQUE_LINES),
      Bucket (HASH_TABLE_SIZE),
      Sym_store (MAX_UNIQUE_LINES2)
   file_mark _
      Text_loc (2, MAX_UNIQUE_LINES)

   common /c1/ Old_count      # separate common because of size limitations
   common /c2/ New_count      # on some machines...
   common /c3/ Old_xref
   common /c4/ New_xref
   common /c5/ Old_lno
   common /c6/ Bucket
   common /c7/ Sym_store
   common /c8/ Text_loc

   sym_pointer _
      Next_sym,
      Next_inx,
      New_size,
      Old_size
   filedes _
      Old_file,
      New_file,
      Text_file,
      Old_copy,
      New_copy
   integer _
      Option,
      Verbose
   character _
      Text_file_name (FILENAMESIZE),
      Old_copy_name (FILENAMESIZE),
      New_copy_name (FILENAMESIZE)

   common /diffcom/ Next_sym, Next_inx, Old_file, New_file, Text_file,
      New_size, Old_size, Old_copy, New_copy, Option, Verbose,
      Text_file_name, Old_copy_name, New_copy_name
#-t-  cdiff                      1096  local   12/03/80  16:26:16
