{application, ridhm_pubsub, [
    {description, "Ridhm Pubsub"},
    {vsn, "1"},
    {modules, ['ridhm_pubsub_app','ridhm_pubsub_handler','ridhm_pubsub_sup']},
    {registered, [ridhm_pubsub_sup]},
    {applications, [
        kernel,
        stdlib,
        cowboy
    ]},
    {mod, {ridhm_pubsub_app, []}},
    {env, []}
]}.
