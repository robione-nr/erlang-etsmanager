# Erlang  ETS Manager

The primary purpose of this module is to preserve ETS table data should the controlling process crash. The ets_supervisor acts as the final back stop. However the ets_manager is the primary module other parts of the code would interact with. This was inspired by Steve Vinoski's article [_Don't Lose Your ets Tables_](http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/).

### Modules
 - ets_supervisor: 

      Prior to starting the ets_manager, it spawns a child process which acts as the heir to the ets_manager's master table. It also monitors the supervisor. Should the supervisor crash the process will terminate.

 - ets_manager: 

      A simple gen_server to retain ownership of any ETS tables from a crashed process. Ready to return them to the proper module per their request upon restart. Should this process crash and restart it will notify any process controlling an ETS table to update the PID of the heir.

      API:
   - create_table/2: Creates a named ETS table with the table options provided and registers it with the manager. It will remove named_table and heir properties set by the user.
   - create_table/3: As above with the addition of heir data.
   - request_table/1: Process requests ownership of a named table from the manager.
   - update_pid/3: Called by receiving process of an 'ETS-NEWMANAGER' message specifying the name, new PID and heir data.
