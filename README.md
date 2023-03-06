# todo

- [x] make sure the gameOrderMetrics calculation works properly
- [x] optimised games shouldn't be green. red is still good for back to back games though.
- [x] Improve comparison of game order metrics and move to gameordermtrics module. If the minimum of the metrics is equal, then the next thing to differentiate on is the average. If the average is also the same, then go for the one with the lowest max (which means that teams will be treated as fairly as possible)
- [x] remove optimise in chunks. it was a good idea, but doesn't work out in practice. Update the optimisation type / model to reflect what it now does.
- [x] Is the 0.2 of the two games rest right? It looks like the number should be a lot higher. braintree 3 single rests, 0.2. Surrey 1 single rest, also 0.2. so yep, definitely wrong. need another test.
- [x] produce something when no good permutation found. Maybe try again with no curtailment, but still with the limit. with test.
- [x] tournament preference scores can be outside of 0 - 1 when teams play consecutively. Just set the score to zero in this case.
- [x] finish optimise page, with good copy and suchlike.
- [x] Add a button to get to edit team page
- [x] and a paste option to games page, so can paste from excel. It needs to work out the teams.
- [x] Choose tournament preference when adding / editing teams
- [x] Add text to games page to say can copy / paste on the page (from numbers, google sheets, excel 365). Maybe mention format
- [x] Add empty state to games page. Have a button to set the games to the default list we currently have. then can remove this from the initial state in the model.
- [x] invalidate optimisation if adding / editing / deleting a game, or editing a team
- [x] Fix this situation: "Showing the best game order I found so far. You can click 'Optimise' again to analyse more options. The team preferences are accommodated with 100-100% success"
- [x] enable / disable routes / links when relevant
- [x] When adding a game, allow entry of a new team (text edit), as well as choosing an existing team
- [x] min width of page
- [ ] add a copy option to the results page
  - probably this will work over https, but it doesn't work locally. test again once deployed. Can test the unhappy path locally and make sure we show a message box or something.
- [ ] anaylse problem edge cases
  - [ ] copy paste with more than two columns
  - [ ] attempt to add / edit such that there are identical teams - not allowed
  - [ ] attempt to add / edit such that there are identical games - this is a possibility and is allowed
  - [ ] attempt to add a game where a team plays itself - not allowed
  - [ ] attempt to add a game without the teams defined
  - [ ] want to be able to cancel editing
  - [ ] attempt to add a team with no name
- [ ] maybe sort out history / routing if I can be bothered
- [ ] do drag drop / touch reordering if possible. This might lead to a 4th page. Maybe "Tweak"
- [ ] run axe / accessibility tools
- [ ] make it easy via keyboard
- [ ] add footer from green pages
  - [ ] google lighthouse

Sort dx. This is all optional and not sure I can be bothered

- [x] elm review
- [ ] add view type
- [ ] split in to smaller files, probably one per uistate, split up the Msg and Model along these lines as well, so top level one is simple
- [ ] use opaque type / parse dont validate where possible, although testing can be an issue
- [ ] add some tests, probably cypress, but also unit tests for anything complicated enough to warrant it

Release

- [ ] create a build, could simply be just on netlify? Maybe something else is better though.
- [ ] Release app to netlify or similar
- [ ] Update read with some friendly text
- [ ] Publicise on canoe polo facebook groups
