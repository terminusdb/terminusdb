#!/bin/bash
# Helper script to list available test suites in both branches

cd "$(dirname "$0")/../.."

echo "Checking test suites in both branches..."
echo ""

# Function to get PL-Unit test suites from a branch
get_plunit_suites() {
    local BRANCH=$1
    git show "$BRANCH:src/interactive.pl" 2>/dev/null | \
        swipl -g "
            open_string('$(<&0)', Stream),
            read_string(Stream, _, Content),
            close(Stream),
            split_string(Content, '\n', '', Lines),
            findall(Suite, (
                member(Line, Lines),
                sub_string(Line, _, _, _, 'begin_tests('),
                split_string(Line, '()', ' \t', Parts),
                member(Part, Parts),
                atom_string(Suite, Part),
                Suite \= ''
            ), Suites),
            sort(Suites, Sorted),
            forall(member(S, Sorted), format('~w~n', [S])),
            halt
        " -t halt 2>/dev/null || echo ""
}

# Get Mocha tests (these are just files)
echo "=== Mocha Tests (JavaScript) ==="
echo "Tests in 128-fix-the-mess:"
git ls-tree 128-fix-the-mess:tests/test/ 2>/dev/null | grep '\.js$' | awk '{print "  " $4}' | head -20
echo ""
echo "Tests in main:"
git ls-tree main:tests/test/ 2>/dev/null | grep '\.js$' | awk '{print "  " $4}' | head -20
echo ""

# Find common Mocha tests
echo "=== Common Mocha Tests (in both branches) ==="
comm -12 \
    <(git ls-tree 128-fix-the-mess:tests/test/ 2>/dev/null | grep '\.js$' | awk '{print $4}' | sort) \
    <(git ls-tree main:tests/test/ 2>/dev/null | grep '\.js$' | awk '{print $4}' | sort) | \
    sed 's/^/  /'
echo ""

# Get PL-Unit test suites
echo "=== PL-Unit Test Suites ==="
echo "Detecting test suites by running on current branch..."
CURRENT_SUITES=$(swipl -g "
    consult('src/interactive.pl'),
    current_module(M),
    module_property(M, class(test)),
    atom_string(M, S),
    sub_string(S, 0, 7, _, 'plunit_'),
    sub_string(S, 7, _, 0, Name),
    format('~w~n', [Name]),
    fail; halt
" -t halt 2>/dev/null | sort)

echo "Available test suites:"
echo "$CURRENT_SUITES" | sed 's/^/  /'
echo ""

echo "=== Suggested Test Configuration ==="
echo ""
echo "Based on tests that handle numeric/document operations:"
echo ""
echo "MOCHA_TESTS=("
comm -12 \
    <(git ls-tree 128-fix-the-mess:tests/test/ 2>/dev/null | grep '\.js$' | awk '{print $4}' | sort) \
    <(git ls-tree main:tests/test/ 2>/dev/null | grep '\.js$' | awk '{print $4}' | sort) | \
    grep -E 'graphql|document|woql|patch|frame|diff|capab|info' | \
    head -10 | \
    sed 's/^/    "/' | sed 's/$/"/'
echo ")"
echo ""
echo "PLUNIT_TESTS=("
echo "$CURRENT_SUITES" | grep -E 'typecast|json|arithmetic|employee|schema|terminus|api|infer' | \
    head -10 | \
    sed 's/^/    "/' | sed 's/$/"/'
echo ")"
