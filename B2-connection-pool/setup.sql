
create table Person(
    name varchar(120) primary key,
    email text not null,
    dob date not null,
    password varchar(120) not null -- don't store plaintext passwords.
);

create table LogEntry(
    author varchar(120) references Person,
    content text not null,
    dayWritten date not null);

insert into Person (name, email, dob, password) values
    ('John', 'John@local.community', '1980-6-23', 'supersecure'),
    ('Susan', 'Susan@company.work', '1990-9-12', 'notReally');

insert into LogEntry (author, content, dayWritten) values
    ('John',  'another day, another dollar','2017 10 29'),
    ('Susan', 'the dog ran away. John was gonna eat it','2017 10 29'),
    ('Susan', 'lunch was good today','2017 10 29'),
    ('John',  'I won 3 rusty cans on the quiz broadcast!','2017 10 22'),
    ('John',  'it is so dark in here','2017 10 22');

