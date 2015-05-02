// Fetch hacker news top stories with Hacker News API
// https://github.com/HackerNews/API

open FSharp.Data

type TopStories = JsonProvider<"https://hacker-news.firebaseio.com/v0/topstories.json">
type Story = JsonProvider<"https://hacker-news.firebaseio.com/v0/item/1.json">

let getStory id =
    sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id
    |> Story.AsyncLoad

[<EntryPoint>]
let main argv =
    TopStories.GetSamples()
    |> Seq.truncate 10
    |> Seq.map getStory
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.iter (fun t -> printfn "%s" t.Title)
    0
