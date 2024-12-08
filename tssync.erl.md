# Add File Sync
Create DiskTableRef (dets) on ts:init

## After ets creation
dets:to_ets(DiskTableRef, TupleSpaceRef (data ets))

## Sync when no message arrives (timeout)

receive
	...
after
	10000 -> sync_data(...), server(...)
end