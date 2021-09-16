# Versioning
TerminusDB uses version numbers major.minor.patch, for example `10.0.1`.
We maintain a release cadence of 1 release a week. Most of these
releases will be patch releases, where we fix some bugs or add new
minor features. The major and minor versions are upped somewhat
arbitrarily, when the team feels like a sufficient milestone has been
reached to warrant such a release.

We maintain releases on branches. Each minor release has its own
branch, for example `10.0`. Any time a new version is released, the
work for this version is merged onto its version branch, and tagged
with the right version number, for example `10.0.3`.

# Issue tracking and scheduling

## The project board
The team keeps track of work through a project board, which can be
found [here](https://github.com/orgs/terminusdb/projects/3). The
project board tracks development over a range of projects, including
the server, the clients, and the documentation.

## Milestones
The team keeps track of work in the server specifically through
milestones corresponding to upcoming releases. At any time the
following milestones will exist:
- the upcoming release, for example `10.1.3`
- the release after that, for example `10.1.4`
- the next minor version, for example `10.2`

When a next minor version is close, an additional minor version
milestone is created for the minor release after that, so we can push
ahead work we do intend to work on very soon, but which won't make it
into the next minor release.

## Monday - planning
Each Monday, the team evaluates the work that needs to be done. The
previous week is reviewed, newly submitted issues are read and
assigned a priority, and a new set of work is decided upon.

New issues are grouped into 'now', 'next' or 'later'. 'Now' is all the
work we expect to go into either the upcoming Thursday release, or
next week's Thursday release. 'next' is work we do not yet anticipate
an exact version number for, but we expect it to be in the same minor
release or the very next minor release (so for example, if we're
currently on `10.2.3`, then 'next' means we anticipate it'll be in
`10.3.0` at the latest). 'later' is anything we wish to work on, but
which we cannot anticipate yet.

Anything that is grouped into 'now' is given a milestone corresponding
to next week's release, unless it is an urgent matter we anticipate
can be done for the upcoming release, in which case it gets assigned
to the milestone for the upcoming release. Anything that is grouped
into 'next' will be assigned to the milestone for the upcoming minor
release. Anything grouped into 'later' is not given a milestone at all.

Monday is also the time to anticipate the upcoming release. If at this
point it seems unlikely that some work under the release milestone
will be done in time, it's moved ahead to the next milestone.

Finally, if there's enough available time to work on extra issues, the
issues in 'next' or even 'later' are considered for inclusion into
'now'.

## Thursday - release
On Thursday, the team makes final decisions on what is to go into the
release. Any work not done by then, which is unlikely to be done in a
very short time, is to be moved to the next release. In exceptional
cases, it may be decided to postpone a release, but generally, even if
there's just one minor change that can make it into the release, we'll
get it released at this point.

In the (European) afternoon, when everything should be done, we merge
the work to the 'canary' branch. Generally this is just a fast-forward
from main, but sometimes it may be a little bit more involved, if some
work on main is not yet release-ready.

After all checks have passed on the canary version (both automated and
our own manual tests), we merge the canary branch to the version
branch, and tag the release.

## Throughout the week
At any time, urgent issues may come up that will affect the upcoming
release. These are then added to the appropriate board category and
milestone without a meeting.

# Releases

As explained above, the team generally releases a patch release every
Thursday. In addition, there are minor and major releases.

## Minor releases
A minor release is ready when there's no longer any issues under its
milestone, and instead, they're all under a patch milestone. If we
encounter this situation on Thursday (or anticipate it on Monday), we
rename the patch milestone to the first release of the minor release,
for example `10.2.0`. On Thursday, this then gets released as the next
minor release on a new version branch.

After a minor release, the team reviews the state of the board,
especially the 'later' category, and determines what will go into the
next minor release.

For a period of time afterwards, additional feature requests may be
added to the next minor release.  After this period, we freeze, at
which point no further feature requests will be scheduled for the next
minor release. At this point, a new milestone is created for the minor
release after that for any postponed work.

During a freeze, it's still possible to postpone work that was
originally planned for this release and push it ahead into the next
release. We just won't take up any extra work.

## Major releases
Major releases are just like minor releases, except the team
arbitrarily decides to call it a major release. This happens when such
a significant amount of work has been done since the last major
release that it makes sense to consider it a major update.
