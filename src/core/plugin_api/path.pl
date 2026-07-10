:- module(plugin_api_path, [
    descriptor_to_path/2,
    validate_db_path/1
]).

:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(core(query/resolve_query_resource), [resolve_absolute_string_descriptor/2]).
:- use_module(library(lists)).

%% descriptor_to_path(+Descriptor, -Path) is det.
%
%  Derives the full graphspec string from a resolved TerminusDB descriptor.
%  Uses resolve_absolute_string_descriptor/2 in reverse mode.
descriptor_to_path(Descriptor, Path) :-
    resolve_absolute_string_descriptor(Path, Descriptor).

%% validate_db_path(+Path) is det.
%
%  Validates that Path conforms to the push driver's input contract.
%  The driver accepts ONLY these forms:
%
%    - 2 segments: "org/db" (shorthand for org/db/local/branch/main)
%    - 5 segments: "org/db/<repo>/branch/<branch>"
%                   or "org/db/<repo>/commit/<commit>"
%      where segment 4 MUST be exactly "branch" or "commit".
%
%  Everything else is REJECTED with a clear error BEFORE any descriptor
%  resolution or network I/O.
%
%  Throws: error(invalid_index_path(Path, Reason), _)
validate_db_path(Path) :-
    (   atom(Path)
    ->  atom_string(Path, Path_String)
    ;   Path_String = Path
    ),
    pattern_string_split("/", Path_String, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    length(Segments, N),
    validate_db_segments(N, Segments, Path).

validate_db_segments(2, [_Org, _DB], _Path) :- !.
validate_db_segments(5, [_Org, _DB, _Repo, Seg4, _Name], Path) :-
    !,
    text_to_string(Seg4, Seg4_Str),
    (   Seg4_Str == "branch"
    ->  true
    ;   Seg4_Str == "commit"
    ->  true
    ;   throw(error(invalid_index_path(Path,
                        bad_segment_4(Seg4, expected_branch_or_commit)), _))
    ).
validate_db_segments(N, _Segments, Path) :-
    throw(error(invalid_index_path(Path,
                    wrong_segment_count(N, expected_2_or_5)), _)).
