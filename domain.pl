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

%% ============================================================
%% Dynamic state
%% ============================================================

:- dynamic rental/5.
%  rental(Id, RentalDate, InventoryId, CustomerId, ReturnDate)
%  ReturnDate = none for active rentals

%% ============================================================
%% Reference data
%% ============================================================

language(1, 'English').
language(2, 'Italian').
language(3, 'Japanese').
language(4, 'French').
language(5, 'German').

category(1, 'Action').
category(2, 'Animation').
category(3, 'Comedy').
category(4, 'Drama').
category(5, 'Horror').
category(6, 'Sci-Fi').
category(7, 'Documentary').
category(8, 'Thriller').

%% ============================================================
%% Actors
%% ============================================================

actor( 1, 'Alex',    'Sterling').
actor( 2, 'Maria',   'Vasquez').
actor( 3, 'David',   'Chen').
actor( 4, 'Sarah',   'Mitchell').
actor( 5, 'Marco',   'Bellini').
actor( 6, 'Yuki',    'Tanaka').
actor( 7, 'Emma',    'Thornton').
actor( 8, 'James',   'O\'Connor').
actor( 9, 'Lisa',    'Park').
actor(10, 'Robert',  'Hayes').
actor(11, 'Anna',    'Kowalski').
actor(12, 'Tom',     'Nakamura').
actor(13, 'Claire',  'Dubois').
actor(14, 'Michael', 'Reed').
actor(15, 'Sofia',   'Reyes').

%% ============================================================
%% Films
%% film(Id, Title, Description, Year, LangId,
%%      RentalDuration, RentalRate, Length, ReplacementCost, Rating)
%% ============================================================

film( 1, 'The Last Horizon',
      'A team of astronauts ventures beyond the known galaxy to find a new home for humanity.',
      2020, 1, 5, 3.99, 142, 24.99, 'PG-13').
film( 2, 'Midnight in Rome',
      'Two strangers meet on a rainy Roman night and discover unexpected connections.',
      2019, 2, 3, 2.99, 118, 19.99, 'R').
film( 3, 'Thunder Road',
      'A retired special forces operative is pulled back into action when his family is threatened.',
      2021, 1, 4, 4.99, 130, 22.99, 'PG-13').
film( 4, 'The Laughing Fox',
      'A cunning trickster outwits a corrupt mayor in this feel-good comedy.',
      2022, 1, 3, 2.99, 98, 17.99, 'PG').
film( 5, 'Shadows of the Deep',
      'Deep-sea researchers encounter ancient horrors in an underwater research station.',
      2020, 1, 5, 3.99, 105, 21.99, 'R').
film( 6, 'Planet Zephyr',
      'Young colonists must survive on an alien world with mysterious indigenous life.',
      2023, 1, 5, 4.99, 136, 24.99, 'PG').
film( 7, 'A Walk in the Rain',
      'An aging professor reflects on his life during a long walk through a stormy city.',
      2018, 4, 3, 2.99, 112, 18.99, 'PG-13').
film( 8, 'Iron Fist Rising',
      'Underground fighters compete in an illegal tournament with deadly stakes.',
      2022, 1, 4, 4.99, 125, 22.99, 'R').
film( 9, 'The Great Escape Artist',
      'A bumbling magician accidentally becomes embroiled in international espionage.',
      2021, 1, 3, 2.99, 104, 17.99, 'PG').
film(10, 'Whispers in the Dark',
      'A detective investigates a series of cryptic messages left at crime scenes.',
      2019, 1, 5, 3.99, 115, 21.99, 'R').
film(11, 'Ocean\'s Fury',
      'Elite navy divers face a rogue submarine commander in the South Pacific.',
      2023, 1, 4, 4.99, 128, 23.99, 'PG-13').
film(12, 'Love in Tokyo',
      'An American photographer and a Japanese chef find love amid cultural differences.',
      2020, 3, 3, 2.99, 110, 18.99, 'PG-13').
film(13, 'The Robot\'s Dream',
      'An animated tale of a robot who learns to dream and discovers what it means to be alive.',
      2022, 1, 3, 2.99, 95, 16.99, 'PG').
film(14, 'Night Terrors',
      'A family moves into a house where the walls remember every terrible thing that happened.',
      2021, 1, 5, 3.99, 99, 20.99, 'R').
film(15, 'Funny Business',
      'A stand-up comedian accidentally exposes a money laundering scheme during a routine.',
      2023, 1, 3, 2.99, 102, 17.99, 'PG-13').
film(16, 'Arctic Survival',
      'A gripping documentary following researchers stranded in the Arctic for six months.',
      2020, 1, 4, 3.99, 88, 19.99, 'PG').
film(17, 'Dragon\'s Keep',
      'An animated epic about a young warrior who befriends the last dragon.',
      2019, 1, 3, 2.99, 108, 18.99, 'PG').
film(18, 'The Silent Witness',
      'A mute witness to a murder must find ways to communicate the truth before the killer strikes again.',
      2022, 1, 5, 4.99, 119, 22.99, 'R').
film(19, 'Summer Daze',
      'Four friends reunite for one last summer vacation that changes everything.',
      2018, 1, 3, 1.99, 96, 15.99, 'PG').
film(20, 'Galactic Pioneers',
      'The first interstellar colony ship faces mutiny, mystery, and the unknown.',
      2024, 1, 5, 4.99, 148, 25.99, 'PG-13').

%% ============================================================
%% Film-Actor relationships
%% ============================================================

film_actor( 1,  1). film_actor( 1,  3). film_actor( 1,  6).
film_actor( 2,  2). film_actor( 2,  5). film_actor( 2, 13).
film_actor( 3,  1). film_actor( 3,  8). film_actor( 3, 10).
film_actor( 4,  7). film_actor( 4,  9). film_actor( 4, 15).
film_actor( 5,  3). film_actor( 5,  4). film_actor( 5, 14).
film_actor( 6,  6). film_actor( 6, 11). film_actor( 6, 12).
film_actor( 7,  5). film_actor( 7, 13). film_actor( 7,  7).
film_actor( 8,  1). film_actor( 8, 10). film_actor( 8, 14).
film_actor( 9,  8). film_actor( 9,  9). film_actor( 9, 15).
film_actor(10,  4). film_actor(10, 10). film_actor(10,  3).
film_actor(11,  1). film_actor(11,  8). film_actor(11, 12).
film_actor(12,  2). film_actor(12,  6). film_actor(12,  9).
film_actor(13,  7). film_actor(13, 11).
film_actor(14,  4). film_actor(14, 14). film_actor(14,  3).
film_actor(15,  9). film_actor(15, 15). film_actor(15,  8).
film_actor(16, 10). film_actor(16, 13).
film_actor(17, 11). film_actor(17, 12). film_actor(17,  7).
film_actor(18,  2). film_actor(18,  4). film_actor(18, 14).
film_actor(19,  5). film_actor(19,  7). film_actor(19, 15).
film_actor(20,  1). film_actor(20,  3). film_actor(20,  6).

%% ============================================================
%% Film-Category relationships
%% ============================================================

film_category( 1, 6).
film_category( 2, 4).
film_category( 3, 1).
film_category( 4, 3).
film_category( 5, 5).
film_category( 6, 6).
film_category( 7, 4).
film_category( 8, 1).
film_category( 9, 3).
film_category(10, 8).
film_category(11, 1).
film_category(12, 4).
film_category(13, 2).
film_category(13, 6).
film_category(14, 5).
film_category(15, 3).
film_category(16, 7).
film_category(17, 2).
film_category(17, 1).
film_category(18, 8).
film_category(19, 3).
film_category(20, 6).

%% ============================================================
%% Customers
%% ============================================================

customer( 1, 'John',    'Smith',    'john.smith@email.com', true).
customer( 2, 'Jane',    'Doe',      'jane.doe@email.com',   true).
customer( 3, 'Bob',     'Johnson',  'bob.j@email.com',      true).
customer( 4, 'Alice',   'Brown',    'alice.b@email.com',    true).
customer( 5, 'Charlie', 'Wilson',   'charlie.w@email.com',  true).
customer( 6, 'Diana',   'Prince',   'diana.p@email.com',    true).
customer( 7, 'Eve',     'Martinez', 'eve.m@email.com',      true).
customer( 8, 'Frank',   'Castle',   'frank.c@email.com',    false).
customer( 9, 'Grace',   'Lee',      'grace.l@email.com',    true).
customer(10, 'Hank',    'Green',    'hank.g@email.com',     true).

%% ============================================================
%% Inventory  (2-3 copies per film, single store)
%% ============================================================

inventory( 1,  1, 1). inventory( 2,  1, 1). inventory( 3,  1, 1).
inventory( 4,  2, 1). inventory( 5,  2, 1).
inventory( 6,  3, 1). inventory( 7,  3, 1). inventory( 8,  3, 1).
inventory( 9,  4, 1). inventory(10,  4, 1).
inventory(11,  5, 1). inventory(12,  5, 1).
inventory(13,  6, 1). inventory(14,  6, 1). inventory(15,  6, 1).
inventory(16,  7, 1). inventory(17,  7, 1).
inventory(18,  8, 1). inventory(19,  8, 1).
inventory(20,  9, 1). inventory(21,  9, 1). inventory(22,  9, 1).
inventory(23, 10, 1). inventory(24, 10, 1).
inventory(25, 11, 1). inventory(26, 11, 1).
inventory(27, 12, 1). inventory(28, 12, 1).
inventory(29, 13, 1). inventory(30, 13, 1). inventory(31, 13, 1).
inventory(32, 14, 1). inventory(33, 14, 1).
inventory(34, 15, 1). inventory(35, 15, 1).
inventory(36, 16, 1). inventory(37, 16, 1).
inventory(38, 17, 1). inventory(39, 17, 1). inventory(40, 17, 1).
inventory(41, 18, 1). inventory(42, 18, 1).
inventory(43, 19, 1). inventory(44, 19, 1).
inventory(45, 20, 1). inventory(46, 20, 1). inventory(47, 20, 1).

%% ============================================================
%% Sample rentals (mix of active and returned)
%% ============================================================

rental( 1, '2026-02-10 14:30',  1,  1, '2026-02-15 10:00').
rental( 2, '2026-02-12 09:00',  6,  2, '2026-02-16 11:30').
rental( 3, '2026-02-14 16:45', 11,  3, none).
rental( 4, '2026-02-15 10:00', 18,  4, none).
rental( 5, '2026-02-16 12:00', 23,  5, none).
rental( 6, '2026-02-17 08:30', 29,  1, none).
rental( 7, '2026-02-11 14:00', 34,  6, '2026-02-14 09:00').
rental( 8, '2026-02-13 17:00', 38,  7, '2026-02-17 16:00').
rental( 9, '2026-02-18 09:15', 45,  9, none).
rental(10, '2026-02-18 11:00',  4, 10, none).

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
            Result = ok(RentalId)
        ;   Result = error('No copies available for this film')
        )
    ;   Result = error('Invalid or inactive customer')
    ).

return_rental(RentalId) :-
    (   retract(rental(RentalId, Date, InvId, CustId, none))
    ->  current_date(Now),
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
