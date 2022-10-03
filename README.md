# Airport Simulation

-----------

## Author: Mahsa Sheikhihafshejani - msheikhihafshejani@smu.edu

-----------

The purpose of simulating this problem is to collect statistics for the following problem:

The operation of a small airport is carried out in two consecutive shifts, each for 8 hours a day. Passengers enter the hall in two different ways. 30% of passengers are type 1, and the rest are type 2. The time between the two arrivals of passengers in the hall has negative exponential distribution with an average of 10 minutes. The arrival takes place from the beginning of the first shift to 45 minutes before the end of the second shift. In fact, no new arrivals are planned from this moment until the end of the second shift, but the arrivals that were planned before this moment must take place.
Type 1 passengers travel the distance from the entrance door of the hall to the delivery location according to a negative exponential distribution with an average of 2.4 minutes, and the rest of the passengers travel this distance according to a negative exponential distribution with an average of 4.4 minutes.
In each work shift, 4 people with identification numbers 1 to 4 serve passengers at the cargo delivery location. Of these people, attendants 1 and 2 serve only type 1 passengers and attendants 3 and 4 serve only type 2 passengers. Over time it has become clear that type 1 passengers prefer to be served by type 1 attendant and only go to attendant #2 when only 2 is available. The same is true for type 2 passengers, i.e. they prefer 3 to 4 unless only 4 is free. Type 1 passengers have their own waiting line, and this is also true for type 2 passengers. Serving type 1 passengers according to the normal distribution with an average of 7 minutes and standard deviation of 1.2 minutes, and type 2 passengers according to a negative exponential distribution with It takes an average of 7 minutes. Check-in passengers leave the lounge evenly distributed between 1 and 2 minutes to get ready to board the plane.
Each of the 4 employees of each shift takes 2 breaks during 8 hours. The first break is 15 minutes and starts 90 minutes after the start of the shift. In fact, the starting moment of the short rest for 4 attendants 1 to 4 is at 90 minutes (for attendants 1 and 3) and 120 (for attendants 2 and 4) so ​​that there is always at least one attendant to serve the passengers. Type 1 and 2 exist. If the employee is not free when starting his break, he must finish his service and then start his break for 15 minutes.
Another break is for eating and lasts 30 minutes. Each of the workers 1 and 3 can start this break exactly 3.5 hours after the start of their shift, provided that they are free, otherwise each of them will take a 30-minute break the first time they are free. When each of these two people returns, his colleague starts his 30-minute break if he is not busy.
Every day a minibus with cargo and passengers comes to the airport. The arrival time of the minibus is uniformly defined between 11 am and 12 noon. The number of minibus passengers is defined according to the Poisson distribution with a mean of 2. All minibus passengers have a higher priority to get out of the waiting line than other Type 2 passengers.

Main questions are:
- The number of cases in which each of the two types of rest is delayed (separately)
- The average duration of each of the above two delays separately
- The working rate of each of the 4 employees separately
- The average length of stay of passengers in the system for type 1, type 2 and minibus passengers separately

