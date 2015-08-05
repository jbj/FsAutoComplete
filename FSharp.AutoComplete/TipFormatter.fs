// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module internal FSharp.InteractiveAutocomplete.TipFormatter

open System.Text
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------

(* This cache is slightly redundant wrt. the one in the global state. It it
 * different, however, in that the second layer of Dictionary is authoritative:
 * if there's a miss then the helptext doesn't exist. *)
let private summaryCache =
  Dictionary<
      string (* xmlfile *),
      Dictionary<string (* signature *), string (* summary *)>
    >()

let getSummaryThroughCache xmlfile signature =
  let signatures =
    match summaryCache.TryGetValue(xmlfile) with
    | true, signatures -> signatures
    | false, _ ->
        let signatures = Dictionary<string, string>()
        let xname s = System.Xml.Linq.XName.Get(s)
        try
          System.Xml.Linq.XElement.Load(xmlfile)
            .Element(xname "members")
            .Elements(xname "member")
          |> Seq.iter (fun membr ->
              try
                signatures.Add(membr.Attribute(xname "name").Value,
                               membr.Element(xname "summary").Value)
              with _ -> ()
            )
        with _ -> ()
        summaryCache.Add(xmlfile, signatures)
        signatures
  match signatures.TryGetValue(signature) with
  | true, summary -> Some summary
  | false, _ -> None

let private buildFormatComment cmt (sb:StringBuilder) =
  match cmt with
  | FSharpXmlDoc.Text s -> sb.AppendLine(s)
  | FSharpXmlDoc.XmlDocFileSignature (file, signature) ->
      (* The documentation may exist in an XML file installed together with the
       * dll. We maintain a cache of all XML files we ever tried to open so we
       * never have to open the same XML file twice. *)
      let xmlfile = if file.EndsWith(".dll")
                    then file.Substring(0, file.Length - 4) + ".xml"
                    else file
      match getSummaryThroughCache xmlfile signature with
      | None -> sb
      | Some summary -> sb.AppendLine(summary)
  | FSharpXmlDoc.None -> sb

// If 'isSingle' is true (meaning that this is the only tip displayed)
// then we add first line "Multiple overloads" because MD prints first
// int in bold (so that no overload is highlighted)
let private buildFormatElement isSingle el (sb:StringBuilder) =
  match el with
  | FSharpToolTipElement.None -> sb
  | FSharpToolTipElement.Single(it, comment) ->
      sb.AppendLine(it) |> buildFormatComment comment
  | FSharpToolTipElement.Group(items) ->
      let items, msg =
        if items.Length > 10 then
          (items |> Seq.take 10 |> List.ofSeq),
            sprintf "   (+%d other overloads)</i>" (items.Length - 10)
        else items, null
      if (isSingle && items.Length > 1) then
        sb.AppendLine("Multiple overloads") |> ignore
      for (it, comment) in items do
        sb.AppendLine(it) |> buildFormatComment comment |> ignore
      if msg <> null then sb.AppendFormat(msg) else sb
  | FSharpToolTipElement.CompositionError(err) ->
      sb.Append("Composition error: " + err)

let private buildFormatTip tip (sb:StringBuilder) =
  match tip with
  | FSharpToolTipText([single]) -> sb |> buildFormatElement true single
  | FSharpToolTipText(its) ->
      sb.AppendLine("Multiple items") |> ignore
      its |> Seq.mapi (fun i it -> i = 0, it) |> Seq.fold (fun sb (first, item) ->
        if not first then sb.AppendLine("\n--------------------\n") |> ignore
        sb |> buildFormatElement false item) sb

/// Format tool-tip that we get from the language service as string
let formatTip tip =
  (buildFormatTip tip (new StringBuilder())).ToString().Trim('\n', '\r').Replace("\r","")
