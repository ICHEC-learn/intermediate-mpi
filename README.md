intermediate-mpi
==================

<!-- Tags: [mpi] -->

This repository containes the slides, practicals and other material used in ICHEC's PRACE Intermediate MPI course. This course targets users who already have some experience with basic MPI and want to deepen their knowledge. It briefly summarises basic concepts and describes them in more detail. Then, more advanced concepts will be described such as defined datatypes, communicator management and one-sided communication. All examples in the course will be done in C. 

Agenda
------

| Day 1 | Left Align |
|:-------:|--------:|
| 10:00-10:10   |  Introduction | 
| 10:10-11:00   |  Introduction to MPI | 
| 11:15-12:00   |  MPI Point-to-Point Communication | 
| 12:10-12:45   |  MPI Collective Communication  | 
| 12:45-13:00   |  Introduction to ICHEC Cluster  | 
| 13:00-14:00   |  LUNCH | 
| 14:00-15:30   |  Practical 1 | 
| 16:00-17:00   |  Practical 2  | 



| Day 2 | Left Align |
|:-------:|--------:|
| 10:00-11:00   |  Groups and Communicators | 
| 11:15-12:00   |  Virtual Topologies  | 
| 12:10-13:00   |  Derived Datatypes  | 
| 13:00-14:00   |  LUNCH  | 
| 14:00-15:30   |  Practical 1  | 
| 16:00-17:00   |  Practical 2  | 



| Day 3 | Left Align |
|:-------:|--------:|
| 10:00-11:00   |  Intercommunicators | 
| 11:15-12:00   |  Persistent Communicators, Packing  | 
| 12:10-13:00   |  One-sided Communications  | 
| 13:00-14:00   |  LUNCH  | 
| 14:00-15:30   |  Practical 1  | 
| 16:00-17:00   |  Practical 2 | 



How to run practicals
---------------------

Practicals will be done on the ICHEC cluster. Participants must have a laptop with a Mac, Linux, or Windows operating system.

* Under Windows 
    * We recommend to install the comfortable tool MobaXterm (https://mobaxterm.mobatek.net/download-home-edition.html) Alternatively  install and run the terminal software putty: https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html.
* Under macOS and Linux 
    * ssh support comes with all distributions 

Logging into Kay (15 minutes)
-----------------------------

You will have been sent a link to submit your ssh keys in the last few days, if you have not submitted your ssh key
you must do so now.

If you have sent your public key in, your ssh key will have been added to your course account. You should **ONLY USE**
**YOUR COURSE ACCOUNT FOR THE DURATION OF THIS COURSE, REGARDLESS IF YOU ALREADY HAVE AN ACCOUNT WITH US**.

Try and log in using the following command. Replace the XX with the course account nthat you have been given

~~~
ssh courseXX@kay.ichec.ie
~~~

You will be asked for a passphrase for your public ssh key. This passphrase is out of ICHEC's control and only you
should know this passphrase, having generated it yourself. It will look similar to below, as it is searching for the
location of your public key in your home directory.

~~~
Enter passphrase for key '/Users/johnsmith/.ssh/id_ed25519':
~~~


Next will be the password provided for this course. If you are accessing this material outside of a course, this will
be your password associated with your account.

~~~
course00@kay.ichec.ie's password:
~~~


Once you get the following output, you have successfully logged in.

~~~
#############################################################################
#      _  __      __     _______ _____ _    _ ______ _____   _____ ______ 
#     | |/ /    /\\ \   / /_   _/ ____| |  | |  ____/ ____| |_   _|  ____|
#     | ' /    /  \\ \_/ /  | || |    | |__| | |__ | |        | | | |__   
#     |  <    / /\ \\   /   | || |    |  __  |  __|| |        | | |  __|  
#     | . \  / ____ \| | _ _| || |____| |  | | |___| |____ _ _| |_| |____ 
#     |_|\_\/_/    \_\_|(_)_____\_____|_|  |_|______\_____(_)_____|______|
#                                                                          
#############################################################################
# 
# You have been successfully logged in to the Kay National HPC System
#
# Documentation: https://www.ichec.ie/academic/national-hpc/kay-documentation
#
# For technical support, email support@ichec.ie
# 
##############################################################################
~~~


Clone the lesson repository
---------------------------

In your current working directory, which should be similar to `course00/home/`, type the following command:

~~~
git clone git@github.com:ICHEC-learn/intermediate-mpi.git
~~~

This will clone the lesson's repository into your current directory, and you have access to all the lecture material
including slides, exercises, practicals and markdown files. You can download this material later with the `scp` command.

