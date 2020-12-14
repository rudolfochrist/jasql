-- name: create-users-table#
-- Create the users table
create table users
(
  _id integer primary key autoincrement,
  username test not null,
  firstname text,
  lastname text
)
;

-- name: bulk-insert-users*!
-- Insert many users
insert into users
(
  username,
  firstname,
  lastname
)
values
(
  :username,
  :firstname,
  :lastname
)
;

-- name: insert-user-returning<!
-- Insert a user
insert into users
(
  username,
  firstname,
  lastname
)
values
(
  :username,
  :firstname,
  :lastname
)
;

-- name: insert-user!
-- Insert a user
insert into users
(
  username,
  firstname,
  lastname
)
values
(
  :username,
  :firstname,
  :lastname
)
;

-- name: update-name!
-- Updates a user's name
update users
set firstname = :firstname,
    lastname = :lastname
where _id = :id
;

-- name: delete-user!
-- Delete user with id
delete from users
where _id = :id
;

-- name: get-single-user?
-- Return single user
select _id, username, firstname, lastname
from users
where _id = :id
;

-- name: get-all-by-lastname
-- List all users with the same lastname
select username, firstname, lastname
from users
where lastname = :lastname
;


