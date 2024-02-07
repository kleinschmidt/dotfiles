get!(ENV, "JULIA_PKG_SERVER", "https://pkg.julialang.org")
get!(ENV, "JULIA_PKG_SERVER_REGISTRY_PREFERENCE", "eager")

try
    using Revise
catch e
    @warn "Error initializing Revise" exception=(e, catch_backtrace())
end
