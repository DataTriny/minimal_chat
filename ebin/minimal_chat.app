{application, minimal_chat,
	[{description, "A minimalist TCP chat server."},
	{vsn, "0.3"},
	{modules, [minimal_chat, mc_client, mc_clients_sup, mc_server, mc_server_sup]},
	{applications, [kernel, stdlib]},
	{mod, {minimal_chat, []}},
	{env, [{name, "Minimal Chat"}, 
		{port, 4200}]}
	]
}.