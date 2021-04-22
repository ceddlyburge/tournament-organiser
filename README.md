# todo

list of teams
- [x] delete a team
- [x] add a team
- [x] edit a team
- [ ] later: remove default initial list of teams before release
- [ ] later: teams can specify whether they want to leave early, arrive late, get games over with as quickly as possible or get as much rest as possible between games. This requires a change to the azure function to be handle the input.

list of games
- [x] generate a list of everyone plays everyone games initially
- [x] delete a game
- [x] add a game
- [x] edit a game
- [ ] not much later: reset / recreate a list of everyone plays everyone games if requested

optimise
- [x] call azure function and decode the results
- [x] display results from azure function
- [ ] highlight teams playing back to back
- [ ] allow reordering
- [ ] maybe later: run the optimisation locally, instead of as an azure function. Maybe can use blazor call c# code locally via web assembly. This looks it might be tricky. Rewriting in Elm is another possibility (and creating a package), but would be a faff. Maybe best just to leave it as an azure function, although it could run up costs, which running locally cannot. Not sure how it would be environmentally. Data centres are optimised and use less power generally, but there is the added network cost.

Sort ux
- [x] show a waiting type thing when waiting for the optimisation api call
- [x] show error message if api call fails
- [ ] show message from the optimisation
- [ ] add error handling for json decoding / mapping
  - [ ] cant decode
  - [ ] decode a team or game that doesn't currently exist
- [ ] do drag drop / touch reordering if possible
- [ ] make look nice, probably try every layout and see if I can do something simple / good with that, otherwise revert to elm-ui probably
- [ ] add routing probably
- [ ] maybe assign each team a colour to make visual comparison easier, in same way that people do in google spreadsheet, might be a faff though, and colours will make the design hard / not work. Maybe there is a better idea that doesn't use colours
- [ ] anaylse problem edge cases
  - attempt to add / edit such that there are identical teams - not allowed
  - attempt to add / edit such that there are identical games - this is a possibility and is allowed
  - attempt to add a game where a team plays itself - not allowed
  - attempt to add a game without the teams defined
  - want to be able to cancel editing
  - attempt to add a team with no name
- [ ] animate the change of the game order when it is optimised. This will be a big job so definitely last thing to do

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
