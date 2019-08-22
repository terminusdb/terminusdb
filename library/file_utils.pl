:- module(file_utils,[
              db_relative_path/1,
              db_path/1,
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
              current_checkpoint_directory/3,
              collections/0,
              collections/1,
              graphs/2,
              make_checkpoint_directory/3,
              ttl_to_hdt/2,
              ntriples_to_hdt/2,
              cleanup_edinburgh_escapes/1,
              last_checkpoint_file/3,
              checkpoint_to_turtle/3
          ]).

:- use_module(utils).

/** <module> File Utils
 * 
 * Utility predicate which help in the manipulation of files and store 
 * some file system constants.
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

/** 
 * db_relative_path(-Path) is det.
 * 
 * Storage location for hdt files 
 */
db_relative_path('/storage/').

/** 
 * db_path(-Path) is det.
 * 
 */
db_path(Path) :-
    once(
        file_search_path(terminus_home,BasePath)
    ), 
    db_relative_path(RelPath),
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
    once(file_search_path(terminus_home,BasePath)),
    db_relative_path(RelPath),
    sanitise_file_name(Collection_ID,CSafe),
    interpolate([BasePath,RelPath,CSafe],Path).

/** 
 * graph_directory(+Collection_ID,+Database_Id:graph_identifier,-Path) is det. 
 * 
 * Returns the path for a given graph 
 */
graph_directory(Collection_ID,G,Path) :-
    once(file_search_path(terminus_home,BasePath)),
    db_relative_path(RelPath),
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
 * last_checkpoint_number(+DatabaseName_Dir_Path,-Number) is det. 
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
 * current_checkpoint_directory(+Collection_ID,+Database_Id:graph_identifer,-Path) is semidet. 
 * 
 * Return the latest checkpoint directory.
 */
current_checkpoint_directory(Collection_ID,Database_Id, Path) :-
    graph_directory(Collection_ID,Database_Id, DatabasePath),
    subdirectories(DatabasePath,AllCheckpoints),
    (   predsort([Delta,X,Y]>>
             (   atom_number(X,XN), atom_number(Y,YN), XN < YN
             ->  Delta=(>)
             ;   Delta=(<)
             ),
             AllCheckpoints,[File|_Rest])
    ->  interpolate([DatabasePath,'/',File],Path)
    ;   throw(no_checkpoint_directory(Database_Id,Path))). 


/**
 * collections is det.
 *
 * Writes a list of the current collections. 
 **/
collections :-
    collections(Cs),
    format('Current Collections: ~q~n', [Cs]).
    
/** 
 * collections(-Collections:list(uri)) is det.
 * 
 * Return a list of all current graphs. 
 * FIX: This is probably a bit dangerous as newly constructed 
 * directories in the hdt dir will *look* like new graphs. 
 * Probably need some sort of metadata. 
 */ 
collections(Collections) :-
    db_path(Collection_Dir), 
    subdirectories(Collection_Dir,Collection_Files),
    include({Collection_Dir}/[Collection_File]>>(
                interpolate([Collection_Dir,Collection_File,'/COLLECTION'], Collection_Marker_Path),
                exists_file(Collection_Marker_Path)
            ), Collection_Files, Valid_Collection_Files), 
    maplist(sanitise_file_name,Collections,Valid_Collection_Files).

/*
 * graphs(?Collection_ID,-Databases:list(uri)) is nondet.
 * 
 * Return a list of all current graphs. 
 * FIX: This is probably a bit dangerous as newly constructed 
 * directories in the hdt dir will *look* like new graphs. 
 * Probably need some sort of metadata. 
 */
graphs(Collection_ID,Databases) :-
    collections(Collections),
    (   member(Collection_ID,Collections)
    ->  sanitise_file_name(Collection_ID,Collection_Name),
        db_path(Path),
        interpolate([Path,Collection_Name], Collection_Path),
        subdirectories(Collection_Path,Database_Names),
        include({Collection_Path}/[Name]>>(
                    interpolate([Collection_Path,'/',Name],X),
                    exists_directory(X)
                ),Database_Names,Valid_Database_Names),
        maplist([N,S]>>sanitise_file_name(N,S),Databases,Valid_Database_Names)
    ;   Databases = []).

/** 
 * make_checkpoint_directory(+Collection_ID,+Database_ID:graph_identifer,-CPD) is det. 
 * 
 * Create the current checkpoint directory
 */
make_checkpoint_directory(Collection_ID, Database_Id, CPD) :-
    graph_directory(Collection_ID,Database_Id, Database_Path),
    ensure_directory(Database_Path),
    last_checkpoint_number(Database_Path,M),
    N is M+1,
    %get_time(T),floor(T,N),
    interpolate([Database_Path,'/',N],CPD),
    make_directory(CPD).


/** 
 * cleanup_edinburgh_escapes(+File) is det.
 * 
 * Removes character escapes of the form '\ddd\' and replaces them 
 * with the re-readable '\ddd'.
 * 
 * Currently does an "in-place" replace.
 */ 
cleanup_edinburgh_escapes(File) :-
    process_create(path(sed), ['-i','s/\\\\\\([0-9][0-9][0-9]\\)\\\\/ /g',File],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["sed killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(Out).
    

/** 
 * ttl_to_hdt(+FileIn,+FileOut) is det.
 * 
 * Create a hdt file from ttl using the rdf2hdt tool.
 */
ttl_to_hdt(FileIn,FileOut) :-
    cleanup_edinburgh_escapes(FileIn),
    process_create(path(rdf2hdt), ['-f','turtle',FileIn,FileOut],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["rdf2hdt killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(Out).

/** 
 * ntriples_to_hdt(+FileIn,-FileOut) is det.
 * 
 * Create a hdt file from ttl using the rdf2hdt tool.
 */
ntriples_to_hdt(FileIn,FileOut) :-
    process_create(path(rdf2hdt), ['-f','ntriples',FileIn,FileOut],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["rdf2hdt killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(Out).

/* 
 * last_checkpoint_file(Collection,File) is det. 
 * 
 * Give the file location of the last checkpoint for 
 * transformation (to turtle json-ld etc). 
 */ 
last_checkpoint_file(C,G,File) :-
    current_checkpoint_directory(C,G,CPD),
    interpolate([CPD,'/1-ckp.hdt'],File).

/** 
 * checkpoint_to_turtle(+C,+G,-Output_File) is det.
 * 
 * Create a hdt file from ttl using the rdf2hdt tool.
 */
checkpoint_to_turtle(Collection,Database,Output_File) :-
    
    last_checkpoint_file(Collection,Database,FileIn), 
    user:file_search_path(terminus_home, Dir),
    get_time(T),floor(T,N),
    interpolate([Dir,'/tmp/',N,'.ntriples'],NTriples_File),
    process_create(path(hdt2rdf), ['-f','ntriples',FileIn,NTriples_File],
                   [ stdout(pipe(NT_Out)),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["hdt2rdf killed with signal ",Signal], M),
        throw(error(M))
    ;   true),
    close(NT_Out),
    
    get_collection_prefix_list(Collection,List),
    prefix_list_to_rapper_args(List,Prefix_Args),
    append([['-i','ntriples','-o','turtle'],Prefix_Args,[NTriples_File]], Args),

    interpolate([Dir,'/tmp/',N,'.ttl'],Output_File),
    open(Output_File, write, Out),

    process_create(path(rapper), Args,
                   [ stderr(null),
                     stdout(stream(Out)),
                     process(Rapper_PID)
                   ]),
    process_wait(Rapper_PID,Rapper_Status),
    (   Rapper_Status=killed(Rapper_Signal)
    ->  interpolate(["hdt2rdf killed with signal ",Rapper_Signal], M),
        throw(error(M))
    ;   true),
    
    delete_file(NTriples_File),
    
    close(Out).
