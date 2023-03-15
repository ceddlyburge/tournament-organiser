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
- [-] add a copy option to the results page
  - probably this will work over https, but it doesn't work locally. test again once deployed. Can test the unhappy path locally and make sure we show a message box or something.
- [x] anaylse problem edge cases
  - [x] copy paste with more than two columns. This works ok, it just doesn't work. Probably a better message would be good. Excel might has 'vs' and suchlike.
  - [x] attempt to add / edit such that there are identical teams - not allowed. It isn't possible to add / edit teams any more.
  - [x] attempt to add / edit such that there are identical games - this is a possibility and is allowed
  - [x] attempt to add a game where a team plays itself - not allowed
  - [x] attempt to add a game without the teams defined
  - [x] attempt to edit a team with no name. It isn't possible to add / edit teams any more.
  - [x] attempt to edit a game where a team plays itself - not allowed
  - [x] attempt to edit a game without the teams defined
  - [x] want to be able to cancel editing a game
  - [x] want to be able to cancel adding a game
- [x] run axe / accessibility tools
- [x] make it easy via keyboard
  - [x] buttons and form controls are all good
  - [x] cant get to hyperlinks with keyboard, hopefully sorted when do routing
- [-] google lighthouse
  - [ ] will mostly have to do once live / on a production build I think
  - [x] should add a meta tag though
  - [x] and use href in the a tags, although there won't be anything to crawl anyway
  - should not allow clicking on the route that you are on, and not have it as an a tag
- [x] maybe sort out history / routing if I can be bothered
  - could go further on this, and store the model in the url, but don't think it is worthwhile
- [x] do drag drop / touch reordering if possible. This might lead to a 4th page. Maybe "Tweak"
  - [x] probably hide on mobile, being as it wont work. Mobile probably pretty unlikely anyway, as people will be copy pasting to excel
  - [x] make copy work, or at least potentially work
  - [x] update tweaks when gameordermetrics changes
  - [x] update stats when tweaks change
  - [x] shows stats on page
  - [x] The team preferences are accommodated with 0-0% success
  - [x] show some help text on page, can drag etc
  - [x] make the nav text smaller, so it always fits on one line
- [x] add footer from green pages

Sort dx. This is all optional and not sure I can be bothered

- [x] elm review
- [ ] add view type
- [ ] split in to smaller files, probably one per uistate, split up the Msg and Model along these lines as well, so top level one is simple
- [ ] use opaque type / parse dont validate where possible, although testing can be an issue
- [ ] add some tests, probably cypress, but also unit tests for anything complicated enough to warrant it

Release

- [ ] create a build, could simply be just on netlify? Maybe something else is better though. have elm review in build.
- [ ] Release app to netlify or similar. maybe render
- [ ] Update readme with some friendly text
- [ ] Publicise on canoe polo facebook groups
- [ ] See if the johnson trotter algorithm is wanted by anyone (or the depth first one I'm actually using)
- [ ] See if can get touch working in dnd package
- [ ] See if can get auto scroll working in dnd package
