% knowledge base
 
% room(Id,Cap,Equipment).
room(z06,10,[projector,hcapped]).
room(z11,10,[smartboard,hcapped]).

% room rules
/**
 these rules is to take neccessary information about room like roomCapacity or roomEquipment
*/
roomCapacity(Id,Capacity) :- room(Id,Capacity,_).
roomEquipment(Id,Equipment) :- room(Id,_,EquList), (var(Equipment), Equipment =EquList; nonvar(Equipment),member(Equipment,EquList)).
roomCourses(Id,Courses) :- courseRoom(Courses,Id).

% course knowledge base

%course(Id,Instructor,Capacity,Hour,Room,Needs).
course(cse341,genc,10,4,z06,[]).
course(cse343,turker,6,3,z11,[]).
course(cse331,bayrakci,5,3,z06,[]).
course(cse321,gozupek,10,4,z11,[]).

% course rules
/**
 these rules is to take neccessary information about courses like courseCapacity or courseNeeds
*/
courseInstructor(Id,Instructor) :- course(Id,Instructor,_,_,_,_).
courseStudents(Id,Students) :- studentCourses(Students,Id). 
courseCapacity(Id,Capacity) :- course(Id,_,Capacity,_,_,_).
courseHour(Id,Hour) :- course(Id,_,_,Hour,_,_).
courseNeeds(Id,Needs) :- course(Id,_,_,_,_,NeedList), (var(Needs), Needs =NeedList ; nonvar(Needs),member(Needs,NeedList)).
courseRoom(Id,Room) :- course(Id,_,_,_,Room,_).

% Occupancy knowledge base
%occupancy(Room,Hour,Course).
occupancy(z06,8,cse341).
occupancy(z06,9,cse341).
occupancy(z06,10,cse341).
occupancy(z06,11,cse341).
occupancy(z06,13,cse331).
occupancy(z06,14,cse331).
occupancy(z06,15,cse331).
occupancy(z11,8,cse343).
occupancy(z11,9,cse343).
occupancy(z11,10,cse343).
occupancy(z11,11,cse343).
occupancy(z11,14,cse321).
occupancy(z11,15,cse321).
occupancy(z11,16,cse321).

% instructor knowledge base
%instuctor(Id,Courses,Needs).
instructor(genc,[cse341],[projector]).
instructor(turker,[cse343],[smartboard]).
instructor(bayrakci,[cse331],[]).
instructor(gozupek,[cse321],[smartboard]).

% instructor rules
/**
 these rules is to take neccessary information about instructor like instructorCourses which can return the courses teacher teachs
*/
instructorCourses(Id,Course) :- instructor(Id,CourseList,_), (var(Course), Course =CourseList; nonvar(Course), member(Course,CourseList)).

% student knowledge base

%student(Id,Courses,Handicapped).
student(1,[cse341,cse343,cse331],no).
student(2,[cse341,cse343],no).
student(3,[cse341,cse331],no).
student(4,[cse341],no).
student(5,[cse343,cse331],no).
student(6,[cse341,cse343,cse331],yes).
student(7,[cse341,cse343],no).
student(8,[cse341,cse331],yes).
student(9,[cse341],no).
student(10,[cse341,cse321],no).
student(11,[cse341,cse321],no).
student(12,[cse343,cse321],no).
student(13,[cse343,cse321],no).
student(14,[cse343,cse321],no).
student(15,[cse343,cse321],yes).

% student rules
/**
 these rules is to take neccessary information about student like studentCourses which can return the courses student takes
*/
studentCourses(Id,Course) :- student(Id,CourseList,_), (var(Course), Course=CourseList ; nonvar(Course), member(Course,CourseList)).

% rules
/**
% Rule to check scheduling conflict
% conflict(Course1,Course2) checks if there is any scheduling conflict between two courses. 
    It will check if their class hours conflicts .   
*/
conflict(Course1,Course2) :- occupancy(_,Hour1,Course1),  occupancy(_,Hour1,Course2), Course1 \= Course2 .

/**
% rule to check if a course can be assigned to a room or which course can be assigned to which room
% assignCourse(z06,cse341) will check if course cse341 can be assigned to the room z06.
% assignCourse(Room,cse341) will check which rooms can course cse341 be assigned.
% assignCourse(Room,Course) will check which room can be assigned to which class.
% if room equipment list is same with the course need list and instructor need list then they can be assigned.  
*/
assign(Room,Course) :- course(Course,Instructor,_,_,_,Needs), instructor(Instructor,_,Needs2), room(Room,_,Equipment), isThere(Needs,Equipment), isThere(Needs2,Equipment).
/**
% isThere is helper rule for assignCourse to check that needs are in the equipment.
*/
isThere([],_).
isThere([H|T],Equipment) :- member(H,Equipment), isThere(T,Equipment).

/**
% rule to check if a student can be assigned to a course or which student can be assigned to which course
% enroll(1,cse341) will check if student id 1 can be assigned to the course cse341.
% enroll(1,Course) will check which courses can student id 1 be assigned.
% enroll(Student,Course) will check which students can be assigned to which courses.
% if student is handicapped then course should be suitable for handicapped student to assign and student courses do not conflict with the course.  
*/
enroll(Student,Course) :- student(Student,CourseList,Handicapped), Handicapped == yes, courseRoom(Course,Room), roomEquipment(Room,Equipment), member(hcapped,Equipment), anyConflict(CourseList,Course).
 
enroll(Student,Course) :- student(Student,CourseList,Handicapped), Handicapped == no, course(Course,_,_,_,_,_), anyConflict(CourseList,Course).

/**
% anyConflict is helper rule for enroll to check that student courses do not conflict with the course.
*/
anyConflict(CourseList,Course) :- member(Course,CourseList).
anyConflict(CourseList,Course) :- checkList(CourseList,Course).
checkList([],_).
checkList([H|T],Course) :- H \= Course, not(conflict(H,Course)), checkList(T,Course).













