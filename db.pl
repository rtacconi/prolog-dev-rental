%% ============================================================
%% Persistence layer: load/save between Prolog and PostgreSQL
%% Generated-app style: durable data in Postgres, logic in domain.pl
%% ============================================================

:- module(db, [
    db_load/0,
    db_persist_rental/5,
    db_persist_return/2
]).

:- use_module(domain).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(string)).

%% Connection from environment (devbox: PGDATABASE=dvdrental, PGHOST=localhost, ...)
db_opts(Opts) :-
    (   getenv('PGDATABASE', Db) -> true ; Db = 'dvdrental' ),
    (   getenv('PGHOST', Host) -> true ; Host = 'localhost' ),
    (   getenv('PGPORT', Port) -> true ; Port = '5432' ),
    (   getenv('PGUSER', User) -> true ; User = '' ),
    Opts = [host(Host), port(Port), db(Db), user(User)].

%% Run psql and return output as list of lines (tab-separated columns)
psql_query(Query, Lines) :-
    db_opts(Opts),
    member(host(H), Opts), member(port(P), Opts), member(db(D), Opts),
    format(atom(PortArg), '~w', [P]),
    process_create(path(psql),
        ['-h', H, '-p', PortArg, '-d', D, '-t', '-A', '-c', Query],
        [stdout(pipe(Out)), process(Pid)]),
    read_stream_to_codes(Out, -1, Codes),
    close(Out),
    process_wait(Pid, _),
    atom_codes(Atom, Codes),
    split_string(Atom, "\n", "", LineAtoms),
    exclude(==(''), LineAtoms, NonEmpty),
    maplist(atom_string, Lines, NonEmpty).

%% Run psql for a command (INSERT/UPDATE); no output needed
psql_command(Command) :-
    db_opts(Opts),
    member(host(H), Opts), member(port(P), Opts), member(db(D), Opts),
    format(atom(PortArg), '~w', [P]),
    process_create(path(psql),
        ['-h', H, '-p', PortArg, '-d', D, '-c', Command],
        [stdout(null), stderr(null), process(Pid)]),
    process_wait(Pid, Exit),
    (   Exit = exit(0) -> true ; throw(db_error(Command, Exit)) ).

%% Escape single quote for SQL
sql_escape(Atom, Escaped) :-
    atom_codes(Atom, Codes),
    maplist(escape_one, Codes, EscapedCodes),
    atom_codes(Escaped, EscapedCodes).
escape_one(0'', [0'', 0'']) :- !.
escape_one(C, [C]).

%% ============================================================
%% Load: clear dynamic facts in domain, then load from Postgres
%% ============================================================

db_load :-
    retractall(domain:rental(_,_,_,_,_)),
    retractall(domain:film(_,_,_,_,_,_,_,_,_,_)),
    retractall(domain:customer(_,_,_,_,_)),
    retractall(domain:inventory(_,_,_)),
    retractall(domain:film_actor(_,_)),
    retractall(domain:film_category(_,_)),
    retractall(domain:actor(_,_,_)),
    retractall(domain:category(_,_)),
    retractall(domain:language(_,_)),
    load_languages,
    load_categories,
    load_actors,
    load_films,
    load_film_actors,
    load_film_categories,
    load_customers,
    load_inventory,
    load_rentals.

load_languages :-
    psql_query("SELECT id, name FROM languages ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, NameS]),
             atom_number(IdA, Id), atom_string(Name, NameS),
             assertz(domain:language(Id, Name)) )).

load_categories :-
    psql_query("SELECT id, name FROM categories ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, NameS]),
             atom_number(IdA, Id), atom_string(Name, NameS),
             assertz(domain:category(Id, Name)) )).

load_actors :-
    psql_query("SELECT id, first_name, last_name FROM actors ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, FirstS, LastS]),
             atom_number(IdA, Id), atom_string(First, FirstS), atom_string(Last, LastS),
             assertz(domain:actor(Id, First, Last)) )).

load_films :-
    psql_query("SELECT id, title, description, year, language_id, rental_duration, rental_rate, length, replacement_cost, rating FROM films ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, TitleS, DescS, YearA, LangA, DurA, RateA, LenA, CostA, RatingA]),
             atom_number(IdA, Id),
             atom_string(Title, TitleS), atom_string(Desc, DescS),
             (YearA = "" -> Year = 0 ; atom_number(YearA, Year)),
             (LangA = "" -> Lang = 1 ; atom_number(LangA, Lang)),
             (DurA = "" -> Dur = 3 ; atom_number(DurA, Dur)),
             (RateA = "" -> Rate = 2.99 ; atom_number(RateA, Rate)),
             (LenA = "" -> Len = 0 ; atom_number(LenA, Len)),
             (CostA = "" -> Cost = 19.99 ; atom_number(CostA, Cost)),
             (RatingA = "" -> Rat = 'PG' ; atom_string(Rat, RatingA)),
             assertz(domain:film(Id, Title, Desc, Year, Lang, Dur, Rate, Len, Cost, Rat)) )).

load_film_actors :-
    psql_query("SELECT film_id, actor_id FROM film_actors ORDER BY film_id, actor_id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [FidA, AidA]),
             atom_number(FidA, Fid), atom_number(AidA, Aid),
             assertz(domain:film_actor(Fid, Aid)) )).

load_film_categories :-
    psql_query("SELECT film_id, category_id FROM film_categories ORDER BY film_id, category_id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [FidA, CidA]),
             atom_number(FidA, Fid), atom_number(CidA, Cid),
             assertz(domain:film_category(Fid, Cid)) )).

load_customers :-
    psql_query("SELECT id, first_name, last_name, email, active FROM customers ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, FirstS, LastS, EmailS, ActiveA]),
             atom_number(IdA, Id),
             atom_string(First, FirstS), atom_string(Last, LastS), atom_string(Email, EmailS),
             (ActiveA = 't' -> Active = true ; Active = false),
             assertz(domain:customer(Id, First, Last, Email, Active)) )).

load_inventory :-
    psql_query("SELECT id, film_id, store_id FROM inventory ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, FidA, SidA]),
             atom_number(IdA, Id), atom_number(FidA, Fid), atom_number(SidA, Sid),
             assertz(domain:inventory(Id, Fid, Sid)) )).

load_rentals :-
    psql_query("SELECT id, rental_date, inventory_id, customer_id, return_date FROM rentals ORDER BY id", Lines),
    forall(member(Line, Lines),
           ( split_string(Line, "\t", "", [IdA, DateS, InvA, CustA, RetA]),
             atom_number(IdA, Id), atom_number(InvA, Inv), atom_number(CustA, Cust),
             atom_string(Date, DateS),
             (RetA = "" -> Ret = none ; atom_string(Ret, RetA)),
             assertz(domain:rental(Id, Date, Inv, Cust, Ret)) )).

%% ============================================================
%% Persist mutations to Postgres
%% ============================================================

db_persist_rental(RentalId, Date, InvId, CustId, _ReturnDate) :-
    sql_escape(Date, DateEsc),
    format(atom(Cmd), 'INSERT INTO rentals (id, rental_date, inventory_id, customer_id, return_date) VALUES (~w, \'~w\', ~w, ~w, NULL)',
           [RentalId, DateEsc, InvId, CustId]),
    psql_command(Cmd).

db_persist_return(RentalId, ReturnDate) :-
    sql_escape(ReturnDate, RetEsc),
    format(atom(Cmd), 'UPDATE rentals SET return_date = \'~w\' WHERE id = ~w', [RetEsc, RentalId]),
    psql_command(Cmd).
