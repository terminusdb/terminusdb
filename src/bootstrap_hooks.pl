/*
 * Shared message hooks for bootstrap.pl and bootstrap_dev.pl
 *
 * These hooks catch and handle warnings that would otherwise crash
 * the compiled binary, particularly HTTP-related errors.
 */

% Ignore warnings from `qsave.pl` that occur on macOS. When the next
% SWI-Prolog is released, it should have the following PR, which would allow us
% to remove this `user:message_hook`.
% <https://github.com/SWI-Prolog/swipl/pull/22>
user:message_hook(qsave(strip_failed(_)), warning, _).

% Catch low level HTTP-related warnings (e.g., malformed cookies) to prevent server crashes
% in the compiled binary. These warnings are logged but don't halt execution.
% This allows the server to gracefully handle invalid cookies like:
%   Cookie: react-resizable-panels:layout=[15,85]; path=/
user:message_hook(http(Term), warning, Lines) :-
    % Use centralized logging from json_log module with fallback
    catch(
        json_log:log_http_warning('HTTP', Term, Lines),
        _Error,
        % Fallback if json_log not loaded yet (early in boot process)
        format(user_error, '~N[WARNING] HTTP: ~w~n', [Term])
    ),
    % Return true to prevent the warning from crashing the server
    true.

% Catch general syntax errors in HTTP headers to prevent crashes
user:message_hook(syntax_error(Term), warning, Lines) :-
    % Check if this is an HTTP-related syntax error by examining the error term
    (   functor(Term, http, _)
    ;   functor(Term, cookie, _)
    ;   functor(Term, header, _)
    ),
    % Use centralized logging with fallback
    catch(
        json_log:log_http_warning('HTTP Syntax Error', Term, Lines),
        _Error,
        format(user_error, '~N[WARNING] HTTP Syntax Error: ~w~n', [Term])
    ),
    % Return true to prevent crash
    true.
