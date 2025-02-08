# ROADMAP 
Roadmap for haskban program 

> Legend for todo list
> x = Finish
> - = partial made

## Version 1.0.0 
  - [ ] Define cli params
    - [x] Get the project name to read and execute render program
    - [x] Show version information
    - [-] Show define commands for haskban program
  
  - [ ] Get the store data from json file (Project)
    - [ ] Define the JSON data structure
    - [ ] Define methods for save and get the project data
    - [?] Map the JSON data into program state 
  
  - [ ] Render panes pictures
    - [ ] Board screen
      - [ ]  Render task / group with borders and apply focus border color when its focused
      - [ ]  Add a horizontal scroll for groups  
      - [ ]  Add a vertical scroll for tasks in group
    - [ ] Form Screen
      - [ ] Form for edit / create group (GroupName)
      - [ ] Form for edit / create Task (TaskTitle, TaskDescription)
  
  - [ ] Manage Events based on the keyboard events
    - [ ]  Move task index Left / Up / Down / Right
    - [ ]  Move group index Left / Up / Down / Right
  
  - [ ] Store a config file for keyboard mapping
  - [ ] Apply logging based on file
