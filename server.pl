%% ============================================================
%% DVD Rental — Web Layer
%%
%% All accidental complexity lives here: HTTP server, routing,
%% HTML rendering, parameter parsing, redirects.
%% ============================================================

:- use_module(domain).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(thread)).

%% ---- Static: serve style.css by absolute path (avoids path resolution 500) ----
:- dynamic css_file_path/1.
:- (  prolog_load_context(directory, D),
      atom_concat(D, '/static/style.css', P),
      assert(css_file_path(P))
  ->  true
  ;   true
  ).
:- http_handler(root('static/style.css'), serve_css, []).

serve_css(Request) :-
    (   css_file_path(P)
    ->  true
    ;   prolog_load_context(directory, D),
        atom_concat(D, '/static/style.css', P),
        assert(css_file_path(P))
    ),
    http_reply_file(P, [mime_type(text/css), unsafe(true)], Request).

%% ---- Routes ----
:- http_handler(root(.),         home_handler,      []).
:- http_handler(root(films),     films_handler,     []).
:- http_handler(root(film),      film_handler,      []).
:- http_handler(root(customers), customers_handler, []).
:- http_handler(root(customer),  customer_handler,  []).
:- http_handler(root(rentals),   rentals_handler,   []).
:- http_handler(root(rent),      rent_handler,      []).
:- http_handler(root('return'),  return_handler,    []).

%% ---- Server control ----

start :- start(8080).
start(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('~n=== DVD Rental running at http://localhost:~w/ ===~n~n', [Port]),
    thread_get_message(_).  % block forever so process stays up when run in background

%% ============================================================
%% Page skeleton
%% ============================================================

page(Title, Content) :-
    reply_html_page(
        [ title(['DVD Rental | ', Title]),
          meta([charset='utf-8']),
          meta([name=viewport,
                content='width=device-width, initial-scale=1']),
          link([rel=stylesheet, href='/static/style.css'])
        ],
        [ \navbar,
          div([class=container], Content),
          \footer
        ]).

navbar -->
    html(nav([class=navbar], [
        div([class='navbar-inner'], [
            a([href='/', class=brand], 'DVD Rental'),
            div([class='nav-links'], [
                a([href='/films'],     'Films'),
                a([href='/customers'], 'Customers'),
                a([href='/rentals'],   'Rentals')
            ])
        ])
    ])).

footer -->
    html(footer([class=footer], 'DVD Rental — statehouse.dev style')).

%% ============================================================
%% Home
%% ============================================================

home_handler(_Request) :-
    stats(FC, CC, AC),
    page('Home', [
        div([class=hero], [
            h1('DVD Rental Store'),
            p('Browse films, manage customers, and track rentals.')
        ]),
        div([class='stats-grid'], [
            \stat_card(FC, 'Films'),
            \stat_card(CC, 'Customers'),
            \stat_card(AC, 'Active Rentals')
        ]),
        div([class='quick-links'], [
            a([href='/films',     class='btn btn-primary'], 'Browse Films'),
            a([href='/customers', class=btn],               'View Customers'),
            a([href='/rentals',   class=btn],               'Manage Rentals')
        ])
    ]).

stat_card(Number, Label) -->
    html(div([class='stat-card'], [
        div([class='stat-number'], Number),
        div([class='stat-label'], Label)
    ])).

%% ============================================================
%% Films (list / search / filter)
%% ============================================================

films_handler(Request) :-
    http_parameters(Request, [
        q(Search,  [default('')]),
        category(CatId, [integer, default(0)])
    ]),
    resolve_films(Search, CatId, Films),
    all_categories(Cats),
    page('Films', [
        h1('Films'),
        \search_bar(Search),
        \category_filter(Cats, CatId),
        \films_table(Films)
    ]).

resolve_films('', 0, Films)     :- !, all_films(Films).
resolve_films('', CatId, Films) :- CatId > 0, !, films_in_category(CatId, Films).
resolve_films(Search, _, Films) :- search_films(Search, Films).

search_bar(Query) -->
    html(form([method='GET', action='/films', class='search-form'], [
        div([class='search-group'], [
            input([type=text, name=q, value=Query,
                   placeholder='Search films by title...', class='search-input']),
            button([type=submit, class='btn btn-primary'], 'Search')
        ])
    ])).

category_filter(Cats, Selected) -->
    { (Selected =:= 0 -> AC = 'cat-link active' ; AC = 'cat-link') },
    html(div([class='category-filter'], [
        a([href='/films', class=AC], 'All'),
        \cat_links(Cats, Selected)
    ])).

cat_links([], _) --> [].
cat_links([category(Id, Name)|Rest], Sel) -->
    { format(atom(Href), '/films?category=~w', [Id]),
      (Id =:= Sel -> C = 'cat-link active' ; C = 'cat-link') },
    html(a([href=Href, class=C], Name)),
    cat_links(Rest, Sel).

films_table([]) -->
    html(p([class='no-results'], 'No films found.')).
films_table(Films) -->
    html(table([class='data-table'], [
        thead(tr([th('Title'), th('Year'), th('Rating'),
                  th('Rate'), th('Available'), th('')])),
        tbody(\film_rows(Films))
    ])).

film_rows([]) --> [].
film_rows([film(Id,T,_,Y,_,_,RR,_,_,Rat)|Rest]) -->
    { format(atom(Href), '/film?id=~w', [Id]),
      format(atom(Rate), '$~2f', [RR]),
      available_count(Id, Avail),
      (Avail > 0 -> AClass = available ; AClass = unavailable) },
    html(tr([
        td(a([href=Href], T)),
        td(Y),
        td(span([class=badge], Rat)),
        td(Rate),
        td(span([class=AClass], Avail)),
        td(a([href=Href, class='btn btn-sm'], 'Details'))
    ])),
    film_rows(Rest).

%% ============================================================
%% Film detail
%% ============================================================

film_handler(Request) :-
    http_parameters(Request, [id(Id, [integer])]),
    (   film(Id, Title, Desc, Year, LangId, RDur, RRate, Len, RCost, Rating)
    ->  language_name(LangId, Lang),
        film_cast(Id, Cast),
        film_category_names(Id, Categories),
        available_count(Id, Avail),
        format(atom(RateStr), '$~2f', [RRate]),
        format(atom(CostStr), '$~2f', [RCost]),
        format(atom(LenStr),  '~w min', [Len]),
        format(atom(DurStr),  '~w days', [RDur]),
        (Avail > 0 -> AClass = available ; AClass = unavailable),
        page(Title, [
            a([href='/films', class='btn btn-sm back-link'], '< Back to Films'),
            div([class='film-detail'], [
                div([class='film-header'], [
                    h1(Title),
                    span([class=badge], Rating)
                ]),
                div([class='film-meta'], [
                    span([class=tag], Lang),
                    span([class=tag], Year),
                    span([class=tag], LenStr),
                    \category_tags(Categories)
                ]),
                p([class='film-desc'], Desc),
                div([class='film-info-grid'], [
                    \info_item('Rental Rate',       RateStr),
                    \info_item('Rental Duration',   DurStr),
                    \info_item('Replacement Cost',  CostStr),
                    div([class='info-item'], [
                        strong('Available Copies'),
                        span([class=AClass], Avail)
                    ])
                ]),
                h2('Cast'),
                \cast_list(Cast),
                \rent_section(Id, Avail)
            ])
        ])
    ;   page('Not Found', [h1('Film not found')])
    ).

info_item(Label, Value) -->
    html(div([class='info-item'], [strong(Label), Value])).

category_tags([]) --> [].
category_tags([Name|Rest]) -->
    html(span([class=tag], Name)),
    category_tags(Rest).

cast_list([]) --> html(p('No cast information available.')).
cast_list(Actors) -->
    html(ul([class='cast-list'], \cast_items(Actors))).

cast_items([]) --> [].
cast_items([actor(_, First, Last)|Rest]) -->
    { format(atom(Name), '~w ~w', [First, Last]) },
    html(li(Name)),
    cast_items(Rest).

rent_section(FilmId, Avail) -->
    { Avail > 0 },
    !,
    html(div([class='rent-section'], [
        h3('Rent This Film'),
        form([method='POST', action='/rent', class='rental-form'], [
            input([type=hidden, name=film_id, value=FilmId]),
            div([class='form-row'], [
                div([class='form-group'], [
                    label([for=customer_id], 'Customer'),
                    select([name=customer_id, id=customer_id,
                            required=required],
                           \customer_options_active)
                ]),
                div([class='form-group form-action'], [
                    button([type=submit, class='btn btn-primary'],
                           'Rent Now')
                ])
            ])
        ])
    ])).
rent_section(_, _) -->
    html(div([class='rent-section'], [
        p([class=warning], 'No copies currently available for rental.')
    ])).

customer_options_active -->
    { all_customers(Cs) },
    customer_opts(Cs).

customer_opts([]) --> [].
customer_opts([customer(Id, F, L, _, true)|Rest]) -->
    { format(atom(Name), '~w ~w', [F, L]) },
    html(option([value=Id], Name)),
    customer_opts(Rest).
customer_opts([_|Rest]) -->
    customer_opts(Rest).

%% ============================================================
%% Customers
%% ============================================================

customers_handler(_Request) :-
    all_customers(Customers),
    page('Customers', [
        h1('Customers'),
        table([class='data-table'], [
            thead(tr([th('ID'), th('Name'), th('Email'),
                      th('Status'), th('')])),
            tbody(\customer_rows(Customers))
        ])
    ]).

customer_rows([]) --> [].
customer_rows([customer(Id, F, L, Email, Active)|Rest]) -->
    { format(atom(Name), '~w ~w', [F, L]),
      format(atom(Href), '/customer?id=~w', [Id]),
      (Active = true -> SC = active, ST = 'Active'
                      ; SC = inactive, ST = 'Inactive') },
    html(tr([
        td(Id),
        td(a([href=Href], Name)),
        td(Email),
        td(span([class=SC], ST)),
        td(a([href=Href, class='btn btn-sm'], 'View'))
    ])),
    customer_rows(Rest).

%% ============================================================
%% Customer detail
%% ============================================================

customer_handler(Request) :-
    http_parameters(Request, [id(Id, [integer])]),
    (   customer(Id, First, Last, Email, Active)
    ->  format(atom(Name), '~w ~w', [First, Last]),
        (Active = true -> StatusText = 'Active' ; StatusText = 'Inactive'),
        customer_rentals(Id, Rentals),
        include(is_active_rental, Rentals, ActiveR),
        exclude(is_active_rental, Rentals, PastR),
        page(Name, [
            a([href='/customers', class='btn btn-sm back-link'],
              '< Back to Customers'),
            div([class='customer-detail'], [
                h1(Name),
                div([class='customer-info'], [
                    p([strong('Email: '), Email]),
                    p([strong('Status: '), StatusText])
                ]),
                h2('Active Rentals'),
                \rental_history_table(ActiveR),
                h2('Rental History'),
                \rental_history_table(PastR)
            ])
        ])
    ;   page('Not Found', [h1('Customer not found')])
    ).

is_active_rental(rental(_, _, _, _, none)).

rental_history_table([]) -->
    html(p([class='no-results'], 'No rentals.')).
rental_history_table(Rentals) -->
    html(table([class='data-table'], [
        thead(tr([th('ID'), th('Film'), th('Rented'),
                  th('Returned'), th('Status')])),
        tbody(\history_rows(Rentals))
    ])).

history_rows([]) --> [].
history_rows([rental(Id, Date, InvId, _, RetDate)|Rest]) -->
    { inventory(InvId, FilmId, _),
      film(FilmId, Title, _, _, _, _, _, _, _, _),
      format(atom(FHref), '/film?id=~w', [FilmId]),
      (RetDate = none
       -> RetStr = '-', SC = active, ST = 'Active'
       ;  RetStr = RetDate, SC = returned, ST = 'Returned') },
    html(tr([
        td(Id),
        td(a([href=FHref], Title)),
        td(Date),
        td(RetStr),
        td(span([class=SC], ST))
    ])),
    history_rows(Rest).

%% ============================================================
%% Rentals (list + new-rental form)
%% ============================================================

rentals_handler(_Request) :-
    active_rentals(Rentals),
    all_customers(Customers),
    all_films(Films),
    page('Rentals', [
        h1('Rentals'),
        div([class=section], [
            h2('New Rental'),
            \new_rental_form(Customers, Films)
        ]),
        div([class=section], [
            h2('Active Rentals'),
            \active_rentals_table(Rentals)
        ])
    ]).

new_rental_form(Customers, Films) -->
    html(form([method='POST', action='/rent', class='rental-form'], [
        div([class='form-row'], [
            div([class='form-group'], [
                label([for=customer_id], 'Customer'),
                select([name=customer_id, id=customer_id,
                        required=required],
                       \cust_opts_from(Customers))
            ]),
            div([class='form-group'], [
                label([for=film_id], 'Film'),
                select([name=film_id, id=film_id,
                        required=required],
                       \film_opts_from(Films))
            ]),
            div([class='form-group form-action'], [
                button([type=submit, class='btn btn-primary'],
                       'Rent Film')
            ])
        ])
    ])).

cust_opts_from([]) --> [].
cust_opts_from([customer(Id, F, L, _, true)|Rest]) -->
    { format(atom(Name), '~w ~w', [F, L]) },
    html(option([value=Id], Name)),
    cust_opts_from(Rest).
cust_opts_from([_|Rest]) -->
    cust_opts_from(Rest).

film_opts_from([]) --> [].
film_opts_from([film(Id, Title, _, _, _, _, _, _, _, _)|Rest]) -->
    { available_count(Id, Avail),
      (Avail > 0
       -> Label = Title, Dis = []
       ;  atom_concat(Title, ' (unavailable)', Label),
          Dis = [disabled=disabled]) },
    html(option([value=Id | Dis], Label)),
    film_opts_from(Rest).

active_rentals_table([]) -->
    html(p([class='no-results'], 'No active rentals.')).
active_rentals_table(Rentals) -->
    html(table([class='data-table'], [
        thead(tr([th('ID'), th('Film'), th('Customer'),
                  th('Rented On'), th('')])),
        tbody(\active_rows(Rentals))
    ])).

active_rows([]) --> [].
active_rows([rental(Id, Date, InvId, CustId, none)|Rest]) -->
    { inventory(InvId, FilmId, _),
      film(FilmId, Title, _, _, _, _, _, _, _, _),
      customer(CustId, CF, CL, _, _),
      format(atom(CustName), '~w ~w', [CF, CL]),
      format(atom(FHref), '/film?id=~w', [FilmId]),
      format(atom(CHref), '/customer?id=~w', [CustId]) },
    html(tr([
        td(Id),
        td(a([href=FHref], Title)),
        td(a([href=CHref], CustName)),
        td(Date),
        td(\return_form(Id))
    ])),
    active_rows(Rest).

return_form(RentalId) -->
    html(form([method='POST', action='/return', class=inline], [
        input([type=hidden, name=rental_id, value=RentalId]),
        button([type=submit, class='btn btn-sm btn-warning'], 'Return')
    ])).

%% ============================================================
%% Rent action (POST)
%% ============================================================

rent_handler(Request) :-
    http_parameters(Request, [
        customer_id(CId, [integer]),
        film_id(FId, [integer])
    ]),
    rent_film(CId, FId, Result),
    (   Result = ok(_)
    ->  throw(http_reply(see_other('/rentals')))
    ;   Result = error(Msg),
        page('Error', [
            div([class='alert error'], [
                h2('Rental Failed'),
                p(Msg),
                a([href='/rentals', class=btn], 'Back to Rentals')
            ])
        ])
    ).

%% ============================================================
%% Return action (POST)
%% ============================================================

return_handler(Request) :-
    http_parameters(Request, [rental_id(RId, [integer])]),
    return_rental(RId),
    throw(http_reply(see_other('/rentals'))).

%% ---- Auto-start when loaded ----
:- start.
