# todo

- [x] make sure the gameOrderMetrics calculation works properly
- [ ] optimised games shouldn't be green. red is still good for back to back games though.
- [ ] finish optimise page, with good copy and suchlike. Maybe say how it is compared to the tournament requests.
- [ ] show a message when no good permutation found. Maybe try again with no curtailment, but still with the limit.
- [ ] enable / disable routes / links when relevant
- [ ] choose tournament preference when adding / editing teams
- [ ] Add an edit team page
- [ ] remove optimise in chunks. it was a good idea, but doesn't work out in practice. Update the optimisation type / model to reflect what it now does.
- [ ] anaylse problem edge cases
  - attempt to add / edit such that there are identical teams - not allowed
  - attempt to add / edit such that there are identical games - this is a possibility and is allowed
  - attempt to add a game where a team plays itself - not allowed
  - attempt to add a game without the teams defined
  - want to be able to cancel editing
  - attempt to add a team with no name
  - optimisation can't create an optimised order.
- [ ] and a paste option to games page, so can paste from excel. It needs to work out the teams.
- [ ] maybe same for teams page, and / or think about which should come first
- [ ] add a copy option to the results page
- [ ] maybe sort out history / routing if I can be bothered
- [ ] do drag drop / touch reordering if possible
- [ ] maybe have an info / help button on each page (could also have a button to auto fill some stuff)

Sort dx. This is all optional and not sure I can be bothered

- [ ] add view type
- [ ] split in to smaller files, probably one per uistate, split up the Msg and Model along these lines as well, so top level one is simple
- [ ] use opaque type / parse dont validate where possible, although testing can be an issue
- [ ] add some tests, probably cypress, but also unit tests for anything complicated enough to warrant it

Release

- [ ] create a build, could simply be just on netlify? Maybe something else is better though.
- [ ] remove default initial list of teams before release
- [ ] Release app to netlify or similar
