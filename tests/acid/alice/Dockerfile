# syntax=docker/dockerfile:1
# ALICE crash-consistency harness for TerminusDB.
#
#   docker build -f tests/acid/alice/Dockerfile -t terminusdb-acid-alice:local .
#   docker run --rm terminusdb-acid-alice:local
#
# ALICE (banks fork, pinned below) records the workload's syscalls with write
# payloads, enumerates every legal crash state of the workload directory, and
# runs checker.py on each reconstructed state. See tests/acid/README.md.
ARG TERMINUSDB_IMAGE=terminusdb/terminusdb-server:local

# --- ALICE toolchain ---------------------------------------------------------
# python:2 gives a Python 2.7 interpreter plus a full build chain on a Debian
# base old enough to satisfy ALICE's requirements.
FROM python:2 AS alice
ARG ALICE_REV=8feae756d8e1a5a2cb46c44762918ec37f3b7ba8
RUN pip install BitVector
# python:2 is buster, which is EOL — the repos live on archive.debian.org now.
RUN sed -i 's|deb.debian.org/debian |archive.debian.org/debian |g; s|security.debian.org/debian-security|archive.debian.org/debian-security|g' /etc/apt/sources.list \
 && printf 'Acquire::Check-Valid-Until "false";\n' > /etc/apt/apt.conf.d/99archive \
 && apt-get update \
 && apt-get install -y --no-install-recommends libunwind-dev liblzma-dev gdb git \
 && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/banks/alice /alice \
 && cd /alice && git checkout "$ALICE_REV"
# alice-strace's -k stacktrace is x86-only upstream; patch in the aarch64
# libunwind path (crash-state checking doesn't depend on the frames).
COPY tests/acid/alice/alice-strace-aarch64.patch /tmp/alice-strace-aarch64.patch
RUN cd /alice && git apply /tmp/alice-strace-aarch64.patch
# Syscalls the tables don't know (rseq et al. on aarch64) must not kill the
# parser — treat them like the "ignore_" prefixed ones.
RUN sed -i 's|startswith("ignore_")|startswith("ignore_") or parsed_line.syscall.startswith("syscall_")|g' \
      /alice/_aliceparsesyscalls.py
# Named syscalls absent from the parser's innocent list that never touch
# the workload dir (observed on aarch64 bookworm).
RUN sed -i "s|^innocent_syscalls += \['mtrace_mmap'.*|&\ninnocent_syscalls += ['prlimit64', 'getrandom', 'statx', 'membarrier', 'sched_getattr', 'setns', 'unshare2']|" \
      /alice/_aliceparsesyscalls.py
# Only a *successful* ioctl on a watched fd can mutate state — ENOTTY
# failures (isatty probes) must not kill the parse.
RUN python - <<'EOF'
p = '/alice/_aliceparsesyscalls.py'
s = open(p).read()
s = s.replace("""\t\tfd = int(parsed_line.args[0])
\t\tassert not fdtracker.is_watched(fd)""",
"""\t\tfd = int(parsed_line.args[0])
\t\tif eval(parsed_line.ret) != -1:
\t\t\tassert not fdtracker.is_watched(fd)""")
open(p, 'w').write(s)
EOF
# mkdirat(AT_FDCWD, ...) — same treatment the fork already gives unlinkat.
RUN python - <<'EOF'
p = '/alice/_aliceparsesyscalls.py'
s = open(p).read()
s = s.replace("""\telif parsed_line.syscall == 'mkdir':""",
"""\telif parsed_line.syscall == 'mkdir' or \\
\t\t(parsed_line.syscall == 'mkdirat' and parsed_line.args[0] == 'AT_FDCWD'):
\t\tif parsed_line.syscall == 'mkdirat':
\t\t\tparsed_line.args.pop(0)""")
open(p, 'w').write(s)
EOF
# F_DUPFD_CLOEXEC is F_DUPFD with a flag — same fd-alias tracking.
RUN sed -i "s|'F_DUPFD', '0x24|'F_DUPFD', 'F_DUPFD_CLOEXEC', '0x24|; s|elif cmd == 'F_DUPFD' and|elif cmd in ['F_DUPFD', 'F_DUPFD_CLOEXEC'] and|" \
      /alice/_aliceparsesyscalls.py
# The store renames over labels it holds open — the op is still recorded for
# reconstruction, so a watched destination must not kill the parse.
RUN sed -i "s|assert len(fdtracker.get_fds(dest_inode)) == 0|pass  # rename over watched dest tolerated|; s|assert memtracker.file_mapped(dest_inode) == False|pass  # rename over mmaped dest tolerated|" \
      /alice/_aliceparsesyscalls.py
# linkat(AT_FDCWD, src, AT_FDCWD, dest, 0) → link(src, dest); drop dirfds+flags.
RUN python - <<'EOF'
p = '/alice/_aliceparsesyscalls.py'
s = open(p).read()
s = s.replace("""\telif parsed_line.syscall == 'link':
\t\tif int(parsed_line.ret) != -1:""",
"""\telif parsed_line.syscall == 'link' or \\
\t\t(parsed_line.syscall == 'linkat' and parsed_line.args[0] == 'AT_FDCWD' and \\
\t\t parsed_line.args[2] == 'AT_FDCWD'):
\t\tif parsed_line.syscall == 'linkat':
\t\t\tparsed_line.args.pop(4)
\t\t\tparsed_line.args.pop(2)
\t\t\tparsed_line.args.pop(0)
\t\tif int(parsed_line.ret) != -1:""")
open(p, 'w').write(s)
EOF
# alice-strace is a modified strace that byte-dumps write payloads; ALICE needs
# the payloads to reconstruct file contents at each simulated crash.
# alice-strace is a modified strace that byte-dumps write payloads; ALICE needs
# the payloads to reconstruct file contents at each simulated crash. On arm64
# the libunwind-ptrace link check needs its transitive deps spelled out.
RUN cd /alice/alice-strace \
 && ./configure LIBS="-lunwind-$(uname -m) -llzma" \
 && make && make install

# --- Harness on top of the server under test ---------------------------------
FROM ${TERMINUSDB_IMAGE}
USER root

# Python 2.7 runtime + alice-strace binaries from the builder stage.
COPY --from=alice /usr/local/ /usr/local/
# alice-strace links libunwind; copy the builder's shared objects rather than
# relying on the runtime image's (differently versioned) libunwind package.
COPY --from=alice /usr/lib/*/libunwind*.so.* /usr/local/lib/
# python2's _hashlib links OpenSSL 1.1, which bookworm no longer ships —
# copy the buster libraries too or every hashlib hash is 'unsupported'.
COPY --from=alice /usr/lib/*/libssl.so.1.1 /usr/lib/*/libcrypto.so.1.1 /usr/local/lib/
RUN ldconfig

COPY --from=alice /alice /alice
ENV ALICE_HOME=/alice
ENV PATH=/alice/bin:/usr/local/bin:$PATH

# python3 is already in the server image and drives driver.py/checker.py.
COPY tests/acid/alice/ /acid/
RUN chmod +x /acid/run.sh /acid/workload.sh /acid/driver.py /acid/checker.py

ENTRYPOINT ["/acid/run.sh"]
