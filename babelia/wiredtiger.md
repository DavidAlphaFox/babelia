
## `(babelia wiredtiger)`

wiredtiger is powerful Ordered Key-Value Store.


### `(wiredtiger-string-error code)`

Return a string representation of the given error `CODE`.

### `(connection-open home config)`

Open a connection at `HOME` directory using `CONFIG`. `CONFIG` must be
a configuration string as documented in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/group__wt.html#gacbe8d118f978f5bfc8ccb4c77c9e8813). Return
a scheme object representing a connection.

### `(connection-close connection config)`

Close `CONNECTION` using `CONFIG`. `CONFIG` must be a configuration
string as documentation in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___c_o_n_n_e_c_t_i_o_n.html#af535c517df851eeac8ebf3594d40b545).

### `(session-open connection config)`

Open a session with `CONNECTION` using `CONFIG`. The returned object
is not thread-safe. `CONFIG` must be a configuration string as
documented in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___c_o_n_n_e_c_t_i_o_n.html#adad5965cd4a60f65b5ac01f7ca6d1fc0).

### `(session-create session name config)`

Create a table, column group, index or file with `SESSION` with `NAME`
and `CONFIG`. `CONFIG` must a be configuration string as documented
in [wiredtiger documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___s_e_s_s_i_o_n.html#a358ca4141d59c345f401c58501276bbb).

### `(session-close session)`

Close the session handle. This will release the resources associated
with the session handle, including rolling back any active
transactions and closing any cursors that remain open in the session.

### `(session-transaction-begin session config)`

Start a transaction. A transaction remains active until ended.
`CONFIG` must a be configuration string as documented in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___s_e_s_s_i_o_n.html#a7e26b16b26b5870498752322fad790bf).

### `(session-transaction-commit session config)`

Commit the current transaction. A transaction must be in progress when
this method is called. If the transaction was rolledback, it will
raise a `wiredtiger` exception with an appropriate code. `CONFIG` must
be a configuration string as documented in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___s_e_s_s_i_o_n.html#a712226eca5ade5bd123026c624468fa2)

### `(session-transaction-rollback session config)`

Rollback the current transaction. A transaction must be in-progress
when this method is called. All cursors are reset. `CONFIG` must be a
configuration string as documented in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___s_e_s_s_i_o_n.html#ab45f521464ad9e54d9b15efc2ffe20a1).

### `(session-reset session)`

Resets all cursors associated with this session and discards cached
resources. The session can be re-used immediately after this call
returns. If a transaction is running on this session, then this call
takes no action and return an error.

### `(cursor-open session uri config)`

Open a cursor at `URI` using `CONFIG`. `CONFIG` must be a
configuration string as documented in [wiredtiger
documentation](http://source.wiredtiger.com/3.2.1/struct_w_t___s_e_s_s_i_o_n.html#afb5b4a69c2c5cafe411b2b04fdc1c75d).

### `(cursor-close cursor)`

Close the cursor. This releases the resources associated with the
cursor handle.  Cursors are closed implicitly by ending the enclosing
connection or closing the session in which they were opened.

### `(cursor-key-format cursor)`

Return a string identifying the column types associated with the key
of `CURSOR`.

### `(cursor-value-format cursor)`

Return a string identifying the column types associated with the value
of `CURSOR`.

### `(cursor-key-ref cursor)`

Return multiple scheme values associated with the key where `CURSOR`
is positioned.

### `(cursor-value-ref cursor)`

Return multiple scheme values associated with the value where `CURSOR`
is positioned.


### `(cursor-key-set cursor . key)`

Set the Set the key for the next operation. If an error occurs during
this operation, a flag will be set in the cursor, and the next
operation to access the value will fail. This simplifies error
handling in applications.

KEY must consistent with the format of the current object key.key for
the next operation. If an error occurs during this operation, a flag
will be set in the cursor, and the next operation to access the value
will fail. This simplifies error handling in applications.

KEY must consistent with the format of the current object key.

### `(cursor-value-set cursor . value)

Set the value for the next operation. If an error occurs during this
operation, a flag will be set in the cursor, and the next operation to
access the value will fail. This simplifies error handling in
applications.

VALUE must consistent with the format of the current object value.

### `(cursor-reset cursor)`

Reset the position of the cursor. Any resources held by the cursor are
released, and the cursor's key and position are no longer valid. A
subsequent iteration with `cursor-next` will move to the first record,
or with `cursor-prev` will move to the last record.

### `(cursor-next? cursor)`

Move cursor to the next record. Return `#t` if the cursor found
it. Otherwise `#f`.

### `(cursor-prev? cursor)`

Move cursor to the previous record. Return `#t` if the cursor found
it. Otherwise `#f`.

### `(cursor-search? cursor)`

On sucess move the cursor to the record matching the key. The key must
first be set.

To minimize cursor resources, the `cursor-reset` method should be
called as soon as the record has been retrieved and the cursor no
longer needs that position.

### `(cursor-search-near cursor)`

Position CURSOR the record matching the key if it exists, or an
adjacent record.  An adjacent record is either the smallest record
larger than the key or the largest record smaller than the key (in
other words, a logically adjacent key).  The key must first be set.

On success, the cursor ends positioned at the returned record; to
minimize cursor resources, the cursor-reset method should be called as
soon as the record has been retrieved and the cursor no longer needs
that position.

Return `'before` if the key is positioned before the requested key,
`'exact` if it is positioned at the given key, `'after` if it is
positioned after the key, otherwise `#f`.

### `(cursor-insert cursor)`

Insert a record and optionally update an existing record.

### `(cursor-update cursor)`

Update a record and optionally insert an existing record.

### `(cursor-remove cursor)`

Remove a record. The key must be set.
