# United Nations Course Tool 

Authors: Amy Stringer and Kerrie Mengersen in collaboration with the Food and Agriculture Organisation of the United Nations. 

# Where is it Deployed? 

A running version of this application can be found [here](https://qutschoolofmaths.shinyapps.io/uncoursetoolapp/).

# Important Note

*An important note to make is that currently this tool is in a proof of concept stage. More work needs to be done before this is likely to be adequately operational.*

# Project Background {.tabset}
## Overall Goal 

The overall goal of this project was to develop a curriculum with the main purpose of upskilling members of the official statistics agencies around the world in the area of remote sensing for agricultural monitoring. 

This is a very broad topic, and in order to develop said curriculum, we needed answers to a few other questions. 

1. Who is likely to need skills in this area? 
2. How can we break "agricultural monitoring" into smaller,more specific skills? 
3. What level of skill is required in these area for the different people undertaking training? And finally, 
4. How can we use courses online, or at other institutions, that already exist to streamline the process? 

## User Profiles 

We brainstormed about who is most likely to benefit from more training in this area, and broadly, we narrowed it down to people who deal with data, and people who make decisions. People who make decisions could be referred to as "Managers" and those with technical background could be separated into "Data Scientists" and "GIS Experts", illustrated in the info graphic below. 

<img src="ImagesForREADME/UserProfiles.png" alt="UserProfiles" width="400"/>

## Knowledge Areas 

After looking around online at what is involved in using remote sensing for agricultural monitoring, we decided that we needed to split things out into key knowledge areas. The goal here is to decide which areas would most likely supplement knowledge of agriculture (which we assume users already have) and allow for better decision making, and practices around monitoring, particularly through the use of satellite imagery. 

This meant people would need to learn about the broad topic of remote sensing and the different ways in which it can be used for agriculture. This led us to consider what is need to handle images produced by satellites, resulting in three more knowledge areas: software, GIS and data management. And finally, we need to equip people with the skills required to derive inference from their data and also visualise and communicate these results, providing justification for data analaysis and visualisation to be added to the key knowledge areas. 

You can see these knowledge areas below with the main goal front and center.  

<img src="ImagesForREADME/CoreSkills.png" alt="KnowledgeAreas" width="500"/>

## Skills Levels 

Once the knowledge areas had been developed, we needed to construct a sort of skill hierarchy. It's unfair to assume that all people wanting to upskill know nothing about any of the knowledge areas. So we need some way to distinguish between those starting out, and those with some experience. 

<img src="ImagesForREADME/SkillLevels.png" alt="SkillLevels" width="500"/>

Above is a figure showing the chosen skill levels. Deciding which content falls into what skill level is a task that requires some extra consideration, and of course, this will vary with the different knowledge areas. 

One approach we could take here is to determine a set of questions for each level of each knowledge area that each level should seek to answer. Give that each of the knowledge areas are fairly broad, these questions will likely need to be broad as well. Below is an example of what this might look like for the remote sensing knowledge area. 

<img src="ImagesForREADME/RemoteSensingFlow.png" alt="RemoteSensingFlow" width="1000"/>

Another option for the skill levels is to adopt the skill level given by the course provider, should the course be one already in existence. 

## The Spine System 

Once we had constructed the user profiles, knowledge areas and skill levels, we had another decision to make. 

Do we create all of this content ourselves? Or do we do our best to make use of the resources that are already out there? 

Our proposition is that we actually construct a hybrid of both methods. That is, we build the foundations, creating what we refer to as the "spine" and then we outsource the more advanced material.

<img src="ImagesForREADME/Spine.png" alt="Spine" width="1000"/>

The idea here is that the orange boxes represent the fundamental level for each of the knowledge areas, and we would build our own fundamentals course that gives the larger picture of what to expect from each of the knowledge areas, and also provide for each knowledge area some key ideas that they may need moving forward.
The white boxes leading down from these are then courses already in existence which build upon fundamental knowledge in the selected area. 

# The Course Tool {.tabset}

After deciding a rough structure for a curriculum, we realised that each person participating would likely be from very different backgrounds, and the course progression one person needs to take may not necessarily suit every other person. From this came the idea of producing a kind of course recommender, or a build-your-own style curriculum. For this to occur, we would need to either find or create some kind of course database, that lists all possible courses one could take in any of our designated knowledge areas, and create is exactly what we did. We developed an application that serves 3 main functions: 

1. A search engine allowing you to search for courses covering particular topics 
2. A course recommendation tool which allows user to build their own personal study profile and download the results 
3. A feedback form allowing users to give feedback on the courses they undertake 

## The Dataset

In order to produce a tool that searched through a database of courses, a database of courses had to first be created. This was done in the following way:

1. Search online for courses in each of the selected knowledge areas 
2. Compile a list of all available courses 
3. Determine suitability of a course based on things such as the provider, the cost, topics covered 
4. For the remaining courses, make a comprehensive (as much as possible) list of topics covered within the course 
5. Determine, based on the content covered, which skill level the course fits into.  
6. Determine, based on the content covered, which user profiles the course is most likely to apply to 
7. Put all of this into a spreadsheet which the following headings: Provider, Company, CourseName, Link, Contents, Level, User. Contents should contain not only keywords about the content in the relevant course, but also the level, user and course name as this is the column that is searched over in the first tab. 

### Course Selection Criteria 

Courses were selected for inclusion based on a number of things. The main thing we made sure of was that the course was offered by a reputable source. In this category we mostly included universities/academic institutions and industry leaders.

Another consideration was the cost. For courses that were offered internally by universities, costs were high and access to information was low and so despite these potentially being very good courses, they were flagged as not easily accessible and therefore excluded. In future, it would be great to form some kind of partnership with universities offering courses in the relevant areas. 

In the end all courses listed in the course database at the present time are free to at least access or audit, with many offering a certificate upon completion for a small fee. 

## The Application 

Please note that this section of the README will code code heavy. 

### Required Files 

In order to run this application on your local machine, you will need to make sure you have all of the required files in the right place. 

Inside the application directory you will need to have: 

- `app.R` - this is the file which contains the majority of the code for the actual application 
- `SearchEngine.R` - this file is used by the `app.R` file and contains the function needed for the search engine component of the app 
- `RecommenderFun.R` - this file is also used by the `app.R` file and contains the function needed for the personalised suggestions component of the app
- `CourseSumDB.csv` - this file is the course database that we search over within the app
- `deploy.R` - This script is not, will not, and should not be contained in the gitHub repo. This script is required to update the hosted version of the application. Should you need this, get it touch and we will go through the approval process. 
  - Alternatively, you can create your own shinyapps.io account and deploy your own version of the application there. 
- A folder called `www` inside the app directory containing the following files `ACEMS.png` and `qut.png`. These are the logo files and any other images you wish to include in the app should also be contained inside the `www` folder. 

### The Search Engine 

#### Inputs 

- `keywords` (textInput) - This is where the user enters the keyword for the course search. The keywords must be entered in a particular format, outlined in the app. Each keyword must be followed by a semi-colon in order for the search function to be effective. 
- `search` (actionButton) - This is the search button. It does not work by simply pressing enter on the keyboard after typing the keywords. The search will not begin until this button is pressed. 

#### Outputs 

- `seachResults` (dataTableOutput) - This presents the results from the keyword search in a pleasing data table. The table presents a selection of the courseDB columns, inclusive of Provider (the online course platform), Company (the company supplying the content), CourseName (the name of the course), Link (a hyperlink leading directly to the course homepage), Level (references the skill level), User (outlines the user profiles the course is best suited for), Contained (indicates how many times a the search function flagged a keyword).

### The Personalised Profile 

#### Inputs 

- `user` (selectInput) - This is a drop down selection allowing users to select which user profile(s) they identify with out of Manager, Data Scientist and GIS Expert. 
- `core` (selectInput) - Another drop down allowing the user to select which knowledge area (or core skill) they are looking to upskill in. User can also select multiple here. Choices include all of the aforementioned knowledge areas. 
- `level` (selectInput) - A drop down box where the user selects what level of knowledge they feel they currently have in the selected knowledge areas. 
- `levelAim` (selectInput) - A drop down box similar to the one above, only this represents the level of knowledge the user is aiming for for undertaking training.
- `search2` (actionButton) - This is a button that initiates a course search based on the criteria selected with the above drop down selections

#### Buttons

- `add` (actionButton) - Allows a user to add the selected rows of the search results to their personal profile
- `remove` (actionButton) - This allows users to, from within the personal profile table, remove courses from their personal list
- `downloadData` (downloadButton) - Allows a user to download a .csv file containing the information from the courses they selected for their personal records. 

#### Outputs

- `recommendations` (dataTableOutput) - provides an interactive table containing the results from the criteria based search. Displayed here are the results from the following logical test (note that this is pseudo-code, this is not what this check looks like in R code) 
    - if `input$user = c("user1", "user2")` and `input$core = c("core1", "core2")` and `input$level = "None" = 0` and `input$levelAim = "Intermediate" = 3` then 
    - we keep results such: From the courses with level between 0 and 3, courses must also satisfy `(("user1" OR "user2") %in% dat$User) AND (("core1" OR "core2") %in% dat$Core)`
    - This is to ensure that if more options are selected from the inputs, then the search results cover a wider range which is contrary to the method used in the search engine portion of the application. This is designed to narrow the search with more input keywords. 
- `profile` (dataTableOutput) - Another interactive table only this time it contains only the courses chosen by the user. This table becomes populated once the user has selected the "Add to cart" button, and each time the button is pressed, more rows may be added (provided they have not already been selected). This is also the table that gets downloaded if the download button is selected. 

### The Feedback Form 

#### Inputs 

Note that all inputs in this tab appear to give qualitative ratings for each question, however, in the backend of the add these are all converted to numerical values from 1 to 5 before being added to the feedback spreadsheet. 

- `courseName` (selectInput) - This is a drop down box allowing the user to select from the list of possible course names in the database. 
- `rat1` (radioButtons) -  This allows the user to give an overall rating of the course before considering specific feedback. The rating here is based on the classic 1-5star system, with 5 being the best outcome. 
- `role1` (radioButtons) - This allows the user to rate how relevant they found the found the course based on their user profile or role. This input allows the user to choose from Irrelevant, Minimally Relevant, Somewhat Relevant, Relevant, Highly Relevant. 
- `level1` (radioButtons) - This input relates to  the pace of the course based on the users starting starting level. This could potentially give insight into which courses have been correctly or incorrectly classified in terms of the level. 
- `clarity` (radioButtons) - This allows the user to rate the clarity of the content on a 5 point scale of Unclear, Somewhat Clear, Unsure, Mostly Clear, and Clear. 
- `delivery` (radioButtons) - This allows the user to rate the delivery on a 1-5 star scale with 5 being the best 
- `feedback` (textarea) - This is where users can enter any specific worded feedback they might have about the course 
- `submit` (actionButton) - To avoid creating an awful feedback dataframe that is updated with each new input, the adding of info has been tied to the submit button. That being said, there is currently nothing in place to prevent user from submitting multiple feedback entries for the same course. 

#### Outputs 

There are no visible outputs for the tab. The results get added to .csv file which is hidden from the user. See where next for more on this. 

# Where next? 

Contained here is a list of potential directions forward with this tool in no particular order. 

- Bring in paid courses. We found some of these courses online and they are contained in the training profiles excel spreadsheet but using the courses in the tool requires some level of partnership with the institutions that offer them as the course materials are not freely available 
- It would be good to automate some of the steps i.e. a rating system for courses that preferences those with better feedback in the recommendation process, and perhaps some web scraping capability for finding any other appropriate courses as they become available. More research would need to be done in order for this to be achieved. 
- Linking feedback to an online excel file or database or Google form so that when feedback is entered via shinyapps.io it actually gets saved somewhere that we can access. Currently, the way that the app is hosted doesn't allow for us to download the updated excel file that theoretically gets built up behind the scenes. 
- Find a way to integrate the regional courses, this requires them being accessible from/available to people in other regions. These, once available, will also need to be summarised in the same way as the other courses and added to the course database
- Storing the courses in an actual online database (rather than an excel file that has been consistently referred to as a database in this file) and updating to code to pull from this, as currently in the deployed version of the application there is no way to really update the course list without modifying the physical spreadsheet and pushing a new version of the app to the server each time we want to add something. These database services are generally not free so this would require some funding to set up and also maintain. 
- Add an option to the feedback form that allows users to enter info from courses which may not be contained within our database. This would be one way for us to find out when new courses should be considered for inclusion. Perhaps we can make it so that this option only becomes available if a user selects "other" from our current course name input and perhaps we could set it up to email somebody whenever a new course is entered so that it is flagged as something to look into. 
- In the feedback form, it may be good to add some text inputs ask questions like 
  - what is your role? (with the hopes that they will be a little more specific than our user profiles) This will allow us to get more of a feel for exactly who is using the tools. 
  - Which knowledge area were hoping to upskill in? This would allow us to validate the feedback. E.g. if they were looking to upskill in data analysis but they were giving feedback about a course that wasn't intended for this area. 
  - What was your starting level? This would allow us to more accurately assess the pace of the courses for the varying levels of knowledge. 
  - etc 
- Implement the spine system - that is, produce video materials that cover the fundamentals across all knowledge areas and add links to these into the spreadsheet/database. The idea here is to create a standardised baseline level that would set users up to understand all the language used in the later level courses, that would also be consistent across globally.  

# FAQs 

These frequently asked questions were taken from an email response sent around by Kerrie. Some of these questions may have been answered in more detail elsewhere in the README. 

1. How did you plan the project? I.e. did you start with competencies, learning outcomes, objectives?
 
Our plan was based on our experience in conducting short courses at the Australian Bureau of Statistics and the UN Big Data Conferences in Colombia and Thailand. These short courses were based on the competencies we found were necessary in the UN Report on EO Data. Lorenzo also contributed his expertise in creating learning material and products for agencies in Africa. Amy developed a lot of the strategy iteratively, as she reviewed courses and their characteristics and content.

We identified the following:

 (i) Classes of learners: Manager, GIS Expert, Data Analyst

 (ii) Fundamental knowledge areas (competencies): Software, Visualisation, Remote Sensing, GIS, Data Management, Data Analysis

 (iii) Learning levels: Fundamental, Introductory, Intermediate, Advanced

 (iv) Objectives: for each Learning level for each Fundamental knowledge area.

2. How does the system address different proficiency levels of learners coming into the platform?
 
There are three proficiency levels, Introductory, Intermediate, Advanced.

3. How did you develop the self-assessment tool? Is it based on something already existing or was it built in-house? How does it map to the courses/competencies?
 
We invented the self-assessment tool and built it in-house. It maps directly to the Classes of learners, Fundamental knowledge areas and Learning levels. It is based on a prototype that we built a few years ago for my postgraduate students in Data Science.

The current format can be seen in the app.

What criteria was used for including certain training courses into the full listing? What was ???in???, what was ???out???, and why.
What was your definition of ???a course????
We included self-contained modules that were openly available, from a reputable source, with clear meta-data that aligned to our learning pathway. 

4. Are ???cases studies???, ???YouTube videos???  (i.e., micro learning) included ?
 
Not at this stage, unless they are embedded in a course.

 

5. What metadata did you capture about each course?
 
Source, Topic, Short description, Target user, Knowledge area, Level, Duration, etc. See app for details.

6. How did you manage with ???series of courses??? that are linked together? Same question with courses needing some prerequisites?
 
They were mapped to the Knowledge areas and Levels.

7. How was the level of training assessed. Has EO TT previously defined minimum requirements for each level? Or maybe the level was defined by the training provider and EO TT adopted it?
 
We adopted the level defined by the training provider and also self-evaluated it to ensure it was consistent with the levels of other courses.

 

8. How were individual roles defined (Manager, GIS Expert, Data Scientist)? What were the criteria for assigning appropriate courses for them?

This was not difficult, given the quite different roles.

9. How did you map the different search criteria to the available training courses?

We used keywords that we devised.

10. Did you review the full training courses or just the short descriptions available?

We initially reviewed the short descriptions then if they appeared to be suitable Amy reviewed the full course. If the full course was unavailable, inclusion depended on how detailed the course description were. If there was not enough detail to determine suitability (i.e. they did not provide, at minimum, a list of topics covered) then the course was not included. 

  a. Was it done manually or was automation used?
Manually for this pilot.

11. How did you assign key words/markers?

Based on our objectives, knowledge areas and levels. We used our experience for this pilot. In a formal rollout, we would take a more rigorous approach, possibly based on a workshop with users/stakeholders and using feedback from the pilot.

12. Did you allow for multiple links for a single course? If so, how did you do this?

Yes. This was not difficult.

13. Does every role that we can choose from Personalized Self Learning System have its own set of skills? What were the criteria for selecting the skills for each role?

Not formally. The criteria were derived from the objectives and knowledge areas. This was more clear for the technical roles, however, the assumption was made that Managers need not upskill in the technical areas. For this reason most courses marked as suitable for Managers don't go beyond the introductory levels in each of the knowledge areas. The reason for this is that the introductory levels provide a broad understanding of the overall topics, where the intermediate and onwards get into specifics. If a manager would like to upskill in specifics for GIS, for example, it may be best to set their role to include GIS Expert as well as manager.  

14. If additional information is received on a course later (via feedback, etc), can the platform be easily updated with the new information?
 
The underpinning platform is a spreadsheet (though online database would be preferred at a later date) so feedback and new courses could be added with ease. A 'star' rating system for courses could also be added, since people are familiar with this. Based on this rating system, courses that are highly rated could be proposed first, and courses that have a low rating could be downgraded or removed.

  a. Which criteria are used to override/change existing            information? (E.g. if a course was classified as "intermediate",     but 3 users say that they consider it as a "beginner" course,     would that be enough to change it?)
 

We haven't got this far in our thinking yet. We have only developed a small pilot.

  b. Is the list of courses constantly updated? What will happen in case of significant modifications to the content of the training, or its cancellation by the provider, will it be automatically updated?

Nothing is automatic at this stage. 

15. What do you see as the next iteration for the product? What kind of feedback have you received?
 
We would like to include courses presented by regional hubs.

We have not socialised this yet, since we wanted to make sure it was aligned with the various UN course and curriculum teams. We would then suggest conducting sessions with users to seek feedback. 

Automating the course audits and database updates would also go a long way, as this is currently a painstaking manual task. 


