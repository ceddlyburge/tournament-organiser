# Tournament organiser

<a href="https://metrics.green-coding.io/ci.html?repo=ceddlyburge/tournament-organiser&amp;branch=2/merge&amp;workflow=51433914"><img src="https://api.green-coding.io/v1/ci/badge/get?repo=ceddlyburge/tournament-organiser&amp;branch=2/merge&amp;workflow=51433914"></a>

Tournament organiser is a website that makes it easy to schedule games in a tournament.

https://tournament-organiser.onrender.com/

## Usage

- Define Games: You can enter the games manually, but probably you already have the list of games in a spreadsheet, in which case copying and pasting them is easier.
- Choose team options: The algorithm will automatically try and prevent any teams playing back to back games, but you can also specify other options for teams, such as wanting to finish all their games as soon as possible (so childrens teams can get back to bed, for example).
- Optimise: Run the algorithm and view the results
- Tweak: Manually change the order of games, to try and capture any team options that the algorithm doesn't yet support (please let us know what these are, and we can add them). The algorithm updates as you make the changes.

## Roadmap

- [ ] Scroll the list while dragging off the screen in the tweak view
- [ ] Add more team options as they are requested

## Tech roadmap

- [ ] Add a test to round trip the model to / from json (this will help ensure that the model is saved to and loaded from local storage successfully)
- [ ] Improve the GameOrderMetricsTests
