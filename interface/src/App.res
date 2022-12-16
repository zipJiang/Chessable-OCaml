// This is a reimplementation to completely rescript-ize the full pipeline

%%raw("import './App.css'")

// Apply a binding to the fetch function

module Response = {
    type t<'data>
    @send external json: t<'data> => Promise.t<'data> = "json"
}

@val
external fetch: (
    string,
    'params
) => Promise.t<Response.t<{"token": Js.Nullable.t<string>, "error": Js.Nullable.t<string>}>> = "fetch"

// And now we try to generate the App
@react.component
let make = () => {

    // let (error, setError) = React.useState(_ => None);
    let (isLoaded, setIsLoaded) = React.useState(_ => false);
    // let (items, setItems) = React.useState(_ => []);

    // Try to define a fetch with useEffect
    React.useEffect(() => {
        open Promise
        let params = {
            "method": "GET"
        }
        let _ = fetch("http://localhost:8080", params)
        -> then(res => {
            Response.json(res)
        })
        -> then(data => {
            switch Js.Nullable.toOption(data["error"]) {
                | Some(msg) => Error(msg)
                | None =>
                    switch Js.Nullable.toOption(data["token"]) {
                        |Some(token) => Ok(token)
                        |None => Error("didn't return a token")
                    }
            }->resolve
        })
        ->catch(e => {
            let msg = switch e {
                | JsError(err) =>
                    switch Js.Exn.message(err) {
                        | Some(msg) => msg
                        | None => ""
                    }
                | _ => "Unexpected error occurred"
            }
            Error(msg)->resolve
        })
        None
    }, );

    <div>
        <Notation comment="This is a great chessgame to observe!">
        </Notation>
    </div>
}