:- module(file_utils,[
              hdt_relative_path/1,
              hdt_path/1,
              touch/1,
              ensure_directory/1,
              sanitise_file_name/2,
              collection_directory/2,
              graph_directory/3,
              subdirectories/2,
              files/2,
              directories/2,
              suffix_length/1,
              type_length/1,
              file_extension_length/1,
              graph_file_timestamp/2,
              graph_file_sequence_number/2,
              last_checkpoint_number/2,
              last_plane_number/2,
              graph_file_type/2,
              graph_file_base/2,
              graph_file_extension/2,
              hdt_file_type/1,
              ntriple_file_type/1,
              turtle_file_type/1,
              graph_file_timestamp_compare/3,
              graph_dir_timestamp_gt/2,
              current_checkpoint_directory/3
          ]).

:- use_module(utils).

/** <module> File Utils
 * 
 * Utilities which help in the manipulation of files. 
 * 
 */

/** 
 * hdt_relative_path(-Path) is det.
 * 
 * Storage location for hdt files 
 */
hdt_relative_path('/hdtdb/').

/** 
 * hdt_path(-Path) is det.
 * 
 */
hdt_path(Path) :-
    once(
        file_search_path(cliopatria,BasePath)
    ), 
    hdt_relative_path(RelPath),
    interpolate([BasePath,RelPath],Path).

/** 
 * touch(+File) is det.
 * 
 * Make an empty file, or change time stamp of current
 */
touch(File) :-
    open(File,append,Stream),
    close(Stream).

/** 
 * ensure_directory(+Path) is det.
 * 
 * Create a directory if it does not already exist
 */
ensure_directory(Directory) :-
    (   exists_directory(Directory)
    ->  true
    ;   make_directory(Directory)).

/** 
 * sanitise_file_name(+G,-F) is det. 
 * 
 * Replace nasty characters. 
/ * WW_FORM encoding seems to work...
 */
sanitise_file_name(G,F) :-
    www_form_encode(G,F).

/** 
 * collection_directory(+Collection_ID,-Path) is det. 
 * 
 * Returns the path for a given graph 
 */
collection_directory(Collection_ID,Path) :-
    once(file_search_path(cliopatria,BasePath)),
    hdt_relative_path(RelPath),
    sanitise_file_name(Collection_ID,CSafe),
    interpolate([BasePath,RelPath,CSafe],Path).

/** 
 * graph_directory(+Collection_ID,+Graph_Id:graph_identifier,-Path) is det. 
 * 
 * Returns the path for a given graph 
 */
graph_directory(Collection_ID,G,Path) :-
    once(file_search_path(cliopatria,BasePath)),
    hdt_relative_path(RelPath),
    sanitise_file_name(G,Gsafe),
    sanitise_file_name(Collection_ID,CSafe),
    interpolate([BasePath,RelPath,CSafe,'/',Gsafe],Path).

/** 
 * subdirectories(+Dir,-Dirs) is semidet.
 * 
 * Show subdirectories only. 
 */
subdirectories(Dir,Dirs) :-
    exists_directory(Dir),
    directory_files(Dir,Files),
    include({Dir}/[File]>>(\+ member(File,['.','..']), 
                           interpolate([Dir,'/',File],FullPath),
                           exists_directory(FullPath)),
            Files,Dirs).

/** 
 * excluded_file(File) is semidet.
 * 
 * Files for exclusion from listings.
 * The categories are exclusive disjunctions, and the 
 * predicate is therefore intended to be semidet.
 */
excluded_file(File) :-
    % emacs files
    atom_concat('.#', _, File).
excluded_file(File) :-
    % current directory and parent directory
    member(File, ['.','..']).

/** 
 * files(+Dir,-NormalFiles) is semidet.
 * 
 * Show normal (non-directory) files only.
 */
files(Dir,NormalFiles) :-
    directory_files(Dir,Files),
    include({Dir}/[File]>>(\+ excluded_file(File),
                           interpolate([Dir,'/',File],FullPath),
                           \+ exists_directory(FullPath)),
            Files,NormalFiles).
/** 
 * directories(+Dir,-Directories) is semidet.
 * 
 * Show directory names only.
 */
directories(Dir, Directories) :-
    directory_files(Dir,Files),
    include({Dir}/[File]>>
            (
                \+ excluded_file(File),
                interpolate([Dir,'/',File],FullPath),
                exists_directory(FullPath)
            ),
            Files, Directories).

/** 
 * suffix_length(-Len) is det.
 *
 * Magic number is len of '-pos.hdt', '-neg.hdt', '-ckp.hdt'
 */
suffix_length(8).

/** 
 * type_length(-Len) is det.
 *
 * Magic number is len of 'pos', 'neg', 'ckp'
 */
type_length(3).

/** 
 * file_extension_length(-Len) is det. 
 * 
 * Magic number is the length of file extensions 'hdt', 'rdf', 'ntr'
 */ 
file_extension_length(3). 

/** 
 * graph_file_timestamp(+File,-Stamp) is det. 
 *
 * Find the timestamp associated with a hdt graph file. 
 */ 
graph_file_timestamp(FilePath,Stamp) :- 
    % Magic number is len of '-pos.hdt', '-neg.hdt', '-ckp.hdt'
    suffix_length(SuffixLength),
    sub_atom(FilePath,0,_Len,SuffixLength,Sub),
    atom_number(Sub,Stamp). 

/** 
 * graph_file_sequence_number(+File,-Number) is det. 
 *
 * Find the sequence number associated with a hdt graph file. 
 */ 
graph_file_sequence_number(FilePath,Seq) :-
    suffix_length(SuffixLength),
    sub_atom(FilePath,0,_Len,SuffixLength,Sub),
    atom_number(Sub,Seq).


/** 
 * last_checkpoint_number(+GraphName_Dir_Path,-Number) is det. 
 *
 * Number of last checkpoint.
 */ 
last_checkpoint_number(Checkpoint_Dir_Path, Seq) :-
    directories(Checkpoint_Dir_Path, Entries),
    maplist([Dir_Num, Num]>>(
                atom_number(Dir_Num, Num)
            ),
            Entries, Sequence),
    sort(Sequence,Sorted),
    debug(last_checkpoint_number, 'Sorted Dirs: ~q~n', [Sorted]),
    (   Sorted = []
    ->  Seq = 0
    ;   reverse(Sorted,[Seq|_])
    ).

/** 
 * last_plane_number(+Checkpoint_Dir_Path,-Number) is det. 
 *
 * Last sequence number
 */ 
last_plane_number(Checkpoint_Dir_Path,Seq) :-
    (   
        files(Checkpoint_Dir_Path, Entries),
        include([File]>>(
                    hdt_file_type(File)
                ;   turtle_file_type(File)
                ), Entries, HDTEntries),
        convlist([F,Num]>>(
                     graph_file_sequence_number(F,Num)
                 ),
                 HDTEntries,Sequence),
        sort(Sequence,Sorted),
        reverse(Sorted,[Seq|_])
    ->  true
    ;   Seq = 0
    ).

/** 
 * graph_file_type(+File,-Type) is semidet.
 * 
 * Return the type of the graph as an atom from the set {pos,neg,ckp}.
 */
graph_file_type(FilePath,Type) :-
    atom_length(FilePath,Length),
    suffix_length(SuffixLength),
    type_length(TypeLength),
    Start is Length - SuffixLength + 1,
    %End is Start + TypeLength,
    sub_atom(FilePath,Start,TypeLength,_End,Type).

/** 
 * graph_file_base(+FilePath,-Base) is semidet. 
 * 
 * Returns the file path without the extension.
 */
graph_file_base(FilePath,Base) :-
    atom_length(FilePath,Length),
    type_length(TypeLength),
    BaseLength is Length - TypeLength - 1,
    sub_atom(FilePath,0,BaseLength,_Length,Base).

/** 
 * graph_file_extension(+FilePath,-Type) is det.
 *
 * Return the three character extension of a file {ntr,ttl,hdt}
 */
graph_file_extension(FilePath,Ext) :-
    atom_length(FilePath,Length),
    file_extension_length(ExtLength),
    Start is Length - ExtLength,
    sub_atom(FilePath,Start,ExtLength,_Length,Ext), !.

/** 
 * hdt_file_type(+File) is semidet. 
 * 
 * Returns true if File is an hdt file. 
 */
hdt_file_type(File) :-
    graph_file_extension(File,hdt).

/** 
 * ntr_file_type(+File) is semidet. 
 * 
 * Returns true if File is an ntriple file. 
 */
ntriple_file_type(File) :-
    graph_file_extension(File,ntr).

/** 
 * turtle_file_type(+File) is semidet. 
 * 
 * Returns true if File is a turtle file. 
 */
turtle_file_type(File) :-
    graph_file_extension(File,ttl).

/** 
 * graph_file_timestamp_compare(+File1,+File2,-Ord) is det.
 * 
 * Checks to see which of the timestamp dates associated with two graph files
 * is greater in order to sort a queue. Ord is (<),(>),(=)
 *
 */
graph_file_timestamp_compare(File1,File2,Order) :-
    graph_file_timestamp(File1,TimeStamp1),
    graph_file_timestamp(File2,TimeStamp2),
    (   TimeStamp1 > TimeStamp2
    ->  Order=(>)
    ;   TimeStamp2 > TimeStamp1
    ->  Order=(<)
    ;   Order=(=)
    ).

/**
 * graph_dir_timestamp_gt(+File1,+File2) is det.
 *
 * Checks to see which of two directory names (which represent timestamps)
 * is greater.
 *
 */
graph_dir_timestamp_gt(Dir1,Dir2) :-
    atom_number(Dir1,Number1),
    atom_number(Dir2,Number2),
    Number1 > Number2. 

/** 
 * current_checkpoint_directory(+Collection_ID,+Graph_Id:graph_identifer,-Path) is semidet. 
 * 
 * Return the latest checkpoint directory.
 */
current_checkpoint_directory(Collection_ID,Graph_Id, Path) :-
    graph_directory(Collection_ID,Graph_Id, GraphPath),
    subdirectories(GraphPath,AllCheckpoints),
    (   predsort([Delta,X,Y]>>
             (   atom_number(X,XN), atom_number(Y,YN), XN < YN
             ->  Delta=(>)
             ;   Delta=(<)
             ),
             AllCheckpoints,[File|_Rest])
    ->  interpolate([GraphPath,'/',File],Path)
    ;   throw(no_checkpoint_directory(Graph_Id,Path))). 
