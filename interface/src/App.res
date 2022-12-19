// This is a reimplementation to completely rescript-ize the full pipeline

%%raw("import './App.css'")

module Response = {
    type t<'data>
    @send external json: t<'data> => Promise.t<'data> = "json"
}

@val
external fetch: (
    string,
    'params
) => Promise.t<Response.t<'a>> = "fetch"

// And now we try to generate the App
@react.component
let make = () => {

    // let (error, setError) = React.useState(_ => None);
    let (isLoaded, setIsLoaded) = React.useState(_ => false);
    let (nodes, setNodes) = React.useState(_ => []);
    let (lines, setLines) = React.useState(_ => []);
    let (in_review, setInReview) = React.useState(_ => []);
    let (c, setC) = React.useState(_ => 0);
    let (transitions, setTransitions) = React.useState(_ => []);

    // Try to define a fetch with useEffect
    React.useEffect(() => {
        let fetchData = () => {
            open Promise
            let params = {
                "method": "GET"
            }
            let _ = fetch("http://localhost:8080", params)
            -> then(res => {
                Response.json(res)
            })
            -> then(data => {
                // Js.Console.log(data["nodes"]);
                setIsLoaded(_ => true);
                setNodes(_ => data["nodes"]);
                // setLines(_ => data["lines"])
                setTransitions(_ => data["transitions"]);

                // We will examine all lines and only set lines to state
                // If the line should be reviewed. 
                let shouldReview = (line) => {
                    switch line {
                    | (l, _) => l["repetition"]["last_seen"] == -1
                    }
                }

                let attach_idx = (line, index) => (line, index)

                setLines(_ => data["lines"])

                Js.Console.log(data["lines"])

                setInReview(_ => {
                    open Js.Array2
                    data["lines"] -> mapi(attach_idx) -> filter(shouldReview)
                });

                Ok(data) -> resolve

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
        }

        if !isLoaded {
            () -> fetchData
        }
        None
    }, );

    if Js.Array2.length(in_review) <= 0 {
        <div>
        {React.string("Waiting for the data to load....")}
        </div>
    }
    else if c < Js.Array2.length(in_review) {

        // Define a function that can set c to next
        let onClick = (evt) => {
            ReactEvent.Mouse.preventDefault(evt)
            setC(c=>c + 1)
        }

        <div>
            <Notation comment="Please make moves for both sides, whether the move is correct or not will be indicated by the outer border of the board.">
            </Notation>

            <Board lines=lines nodes=nodes transitions=transitions line_select=c key={Belt.Int.toString(c)} onClick=onClick>
            </Board>
        </div>
    }
    else {
        <div>
            <Notation comment="Please make moves for both sides, whether the move is correct or not will be indicated by the outer border of the board.">
            </Notation>
            <div>{React.string("Finished all lines")}</div>
        </div>
    }
}