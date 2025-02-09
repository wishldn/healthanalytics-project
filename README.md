# Introduction to Health Analytics Group Project template
Outline for Introduction to Health Analytics student group project

## Setup Instructions
1. One person from the group should:
    - Fork this repository by following instructions [here](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo). Make sure you rename it!
    - Add the other members of your team to the repository by following the instructions [here](https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-personal-account-on-github/managing-access-to-your-personal-repositories/inviting-collaborators-to-a-personal-repository).

2. All members of the group should then:
    - Sign in to Github and Github Desktop
    - Clone the forked repository to your local computer and open in Github Desktop by following the instructions [here](https://docs.github.com/en/desktop/adding-and-cloning-repositories/cloning-a-repository-from-github-to-github-desktop).
    - Make a change to your local copy of the repo (e.g. add a test file), commit that change and then push to the master using the instructions [here](https://docs.github.com/en/desktop/making-changes-in-a-branch/committing-and-reviewing-changes-to-your-project-in-github-desktop). Ignore the information about branches - for now you can just work on the main branch.

## Description of data
In this section you should describe what data you use for your project with enough detail that anyone could download it and run your analysis.
- **IPUMS series**: IPUMS NATIONAL HEALTH INTERVIEW SERIES
- **Countries**: United States of America
- **Years**: 2015, 2016, 2017, 2018
- **How to access the data**: This analysis utilized the IPUMS NHIS data from United States (URL:https://nhis.ipums.org/nhis/), selecting individual survey information from 2015 to 2018 to ensure data availability and integrity, the variables were selected from the IPUMS include:"AHOPELESS", "ANERVOUS", "ANERVOUS", "ARESTLESS", "ASAD", "AWORTHLESS", "AEFFORT", "CIGSDAY", "AGE", "SEX", "INFAM07ON","HEALTH","NCHILD", "COHABEVMAR", "EDUCREC1","EMPSTATIMP1","INCFAM97ON2", "CIGDAYMO", "HRSLEEP","SLEEPFALL", "SLEEPSTAY". Variables include "AHOPELESS", "ANERVOUS", "ANERVOUS", "ARESTLESS", "ASAD", "AWORTHLESS", "AEFFORT", "CIGSDAY", "AGE", "SEX", "INFAM07ON"and "HEALTH","NCHILD" were applied for main regression model, while "SLEEPFALL" and "SLEEPSTAY" were used for robustness check. Due to time constraint of the analysis, all other variables were not applied but considered to be used for robustness check if time permit. 

  

## Description of how to run the code
Here you should explain how someone could replicate the analysis in your report. If there are several code files, explain what each of them does.
