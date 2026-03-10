get!(ENV, "JULIA_PKG_SERVER", "https://pkg.julialang.org")
get!(ENV, "JULIA_PKG_SERVER_REGISTRY_PREFERENCE", "eager")

get!(ENV, "JULIA_PKG_USE_CLI_GIT", true)

try
    using Revise
catch e
    @warn "Error initializing Revise" exception=(e, catch_backtrace())
end

function datastore_creds()
    ENV["DATASTORE_USERNAME"] = readchomp(`op read "op://Employee/Beacon keycloak/username"`)
    ENV["DATASTORE_PASSWORD"] = readchomp(`op read "op://Employee/Beacon keycloak/password"`)
    return nothing
end
