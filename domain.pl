:- module(domain, [
    film/10,
    actor/3,
    category/2,
    language/2,
    customer/5,
    inventory/3,
    film_actor/2,
    film_category/2,
    rental/5,
    all_films/1,
    search_films/2,
    films_in_category/2,
    film_cast/2,
    film_category_names/2,
    all_customers/1,
    all_categories/1,
    active_rentals/1,
    customer_rentals/2,
    language_name/2,
    available_count/2,
    is_available/1,
    stats/3,
    rent_film/3,
    return_rental/1
]).

:- use_module(library(lists)).
:- use_module(db).

%% ============================================================
%% Dynamic state (loaded from PostgreSQL by db.pl)
%% ============================================================

:- dynamic rental/5.
:- dynamic film/10, actor/3, category/2, language/2, customer/5,
   inventory/3, film_actor/2, film_category/2.
%  rental(Id, RentalDate, InventoryId, CustomerId, ReturnDate)
%  ReturnDate = none for active rentals

%% Reference and entity data are loaded by db:db_load/0 from PostgreSQL.
%% No static facts below — this is a generated-app style (durable data in DB).

%% ============================================================
%% Queries
%% ============================================================

all_films(Films) :-
    findall(film(Id,T,D,Y,L,RD,RR,Len,RC,Rat),
            film(Id,T,D,Y,L,RD,RR,Len,RC,Rat),
            Films).

search_films(Query, Films) :-
    downcase_atom(Query, LQ),
    findall(film(Id,T,D,Y,L,RD,RR,Len,RC,Rat),
            ( film(Id,T,D,Y,L,RD,RR,Len,RC,Rat),
              downcase_atom(T, LT),
              sub_atom(LT, _, _, _, LQ) ),
            Films).

films_in_category(CatId, Films) :-
    findall(film(Id,T,D,Y,L,RD,RR,Len,RC,Rat),
            ( film_category(Id, CatId),
              film(Id,T,D,Y,L,RD,RR,Len,RC,Rat) ),
            Films).

film_cast(FilmId, Actors) :-
    findall(actor(AId, First, Last),
            ( film_actor(FilmId, AId), actor(AId, First, Last) ),
            Actors).

film_category_names(FilmId, Names) :-
    findall(Name,
            ( film_category(FilmId, CId), category(CId, Name) ),
            Names).

all_customers(Customers) :-
    findall(customer(Id,F,L,E,A), customer(Id,F,L,E,A), Customers).

all_categories(Cats) :-
    findall(category(Id,N), category(Id,N), Cats).

active_rentals(Rentals) :-
    findall(rental(Id, Date, InvId, CustId, none),
            rental(Id, Date, InvId, CustId, none),
            Rentals).

customer_rentals(CustId, Rentals) :-
    findall(rental(Id, Date, InvId, CustId, Ret),
            rental(Id, Date, InvId, CustId, Ret),
            Rentals).

language_name(LangId, Name) :-
    language(LangId, Name), !.
language_name(_, 'Unknown').

available_count(FilmId, Count) :-
    findall(InvId, available_inventory(FilmId, InvId), Ids),
    length(Ids, Count).

is_available(FilmId) :-
    available_inventory(FilmId, _), !.

available_inventory(FilmId, InvId) :-
    inventory(InvId, FilmId, _),
    \+ rental(_, _, InvId, _, none).

stats(FilmCount, CustomerCount, ActiveCount) :-
    findall(_, film(_,_,_,_,_,_,_,_,_,_), Fs), length(Fs, FilmCount),
    findall(_, customer(_,_,_,_,_), Cs),       length(Cs, CustomerCount),
    findall(_, rental(_,_,_,_,none), Rs),       length(Rs, ActiveCount).

%% ============================================================
%% Commands (mutations)
%% ============================================================

rent_film(CustomerId, FilmId, Result) :-
    (   customer(CustomerId, _, _, _, true)
    ->  (   available_inventory(FilmId, InvId)
        ->  next_id(RentalId),
            current_date(Now),
            assertz(rental(RentalId, Now, InvId, CustomerId, none)),
            db:db_persist_rental(RentalId, Now, InvId, CustomerId, none),
            Result = ok(RentalId)
        ;   Result = error('No copies available for this film')
        )
    ;   Result = error('Invalid or inactive customer')
    ).

return_rental(RentalId) :-
    (   retract(rental(RentalId, Date, InvId, CustId, none))
    ->  current_date(Now),
        db:db_persist_return(RentalId, Now),
        assertz(rental(RentalId, Date, InvId, CustId, Now))
    ;   true
    ).

%% ============================================================
%% Internal helpers
%% ============================================================

next_id(Next) :-
    findall(Id, rental(Id,_,_,_,_), Ids),
    (   Ids = []
    ->  Next = 1
    ;   max_list(Ids, Max), Next is Max + 1
    ).

current_date(DateAtom) :-
    get_time(Now),
    format_time(atom(DateAtom), '%Y-%m-%d %H:%M', Now).
