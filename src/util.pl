max_list(List, Max) :-
    member(Max, List), 
    \+((member(Element, List), Element > Max)).
    