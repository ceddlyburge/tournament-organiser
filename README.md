# todo

Sort ux

- [x] Add link to Define Teams at top / sort out navigation
- [x] sort out "Define Games" page
- [ ] sort out "Add Game" page
- [ ] sort out "Edit Game" page
- [ ] sort out Optimise page
- [ ] maybe sort out history / routing if I can be bothered
- [ ] Do hover over for Add team button and delete buttons
- [ ] Improve display of loading graphic (maybe grey out screen and overlay or similar)
- [ ] Improve display of api failure (maybe it pops up and then fades away)
- [ ] Improve display of optimisation message. Only show if there is one, and put it in the right place (above list). Remove it if the list is reordered (when reordering is possible)
- Add "Analyse" button, to show the optimsation metrics for the currently displayed game order
- [ ] add error handling for json decoding / mapping
  - [ ] cant decode
  - [ ] decode a team or game that doesn't currently exist
- [ ] do drag drop / touch reordering if possible
- [ ] add routing probably. not sure can be arsed though.
- [ ] maybe assign each team a colour to make visual comparison easier, in same way that people do in google spreadsheet, might be a faff though, and colours will make the design hard / not work. Maybe there is a better idea that doesn't use colours
- [ ] anaylse problem edge cases

  - attempt to add / edit such that there are identical teams - not allowed
  - attempt to add / edit such that there are identical games - this is a possibility and is allowed
  - attempt to add a game where a team plays itself - not allowed
  - attempt to add a game without the teams defined
  - want to be able to cancel editing
  - attempt to add a team with no name
  - optimisation can't create an optimised order. It returns a bool for this, but maybe returns an empty list as well, so need to make sure that we don't wipe our our existing game list in this case

- [ ] before release: remove default initial list of teams before release
- [ ] much later: teams can specify whether they want to leave early, arrive late, get games over with as quickly as possible or get as much rest as possible between games. This requires a change to the azure function to be handle the input.
- [ ] not much later: reset / recreate a list of everyone plays everyone games if requested

- [x] highlight teams playing back to back. The api tells us this, but maybe we should work it out ourselves. It isn't super complicated, and avoids repeated calls to the api. However, we might want other stats from the optimisation, such as how many teams are playing back to back, and how well the optimisation meets the requirements. hmmm. I think adding an "Analyse" button, and corresponding endpoint on the azure function is the best way to do this.
- [ ] allow reordering
- [ ] maybe later: run the optimisation locally, instead of as an azure function. Blazor doesn't look easy, and has a very big download size so not viable I think. Rewriting in Elm is probably better (and maybe creating a package), but would be a faff. Maybe best just to leave it as an azure function, although it could run up costs, and comes with network edge cases to deal with, which running locally cannot. Not sure how it would be environmentally. Data centres are optimised and use less power generally, but there is the added network cost.

Sort dx

- [ ] add view type
- [ ] split in to smaller files, probably one per uistate, split up the Msg and Model along these lines as well, so top level one is simple
- [ ] split out code related to the azure function / api
- [ ] use opaque type / parse dont validate for game list (and hide order as an implentation detail)
- [ ] add some tests, probably cypress, but also unit tests for anything complicated enough to warrant it

Release

- [ ] Release azure function
- [ ] Release app to netlify
- [ ] Azure function to require a token
- [ ] Add netlify function / aws lambda to hide the token for the azure function and use
