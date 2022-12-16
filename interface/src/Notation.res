//Implement a notation window that displays the current board position, comments etc.
@react.component
let make = (~comment: string) => {
    /* TODO: implement some state options for interaction
    */
    <div className="annotation-container">
        <p>{React.string(comment)}</p>
    </div>
}