import { int32ToString, createAtom } from "./.fable/fable-library.3.1.5/Util.js";
import { toString, Union, Record } from "./.fable/fable-library.3.1.5/Types.js";
import { union_type, list_type, tuple_type, int32_type, record_type, class_type, string_type } from "./.fable/fable-library.3.1.5/Reflection.js";
import { cons, map, empty } from "./.fable/fable-library.3.1.5/List.js";
import { newGuid } from "./.fable/fable-library.3.1.5/Guid.js";
import { defaultArg } from "./.fable/fable-library.3.1.5/Option.js";
import { chunkBySize, fill } from "./.fable/fable-library.3.1.5/Array.js";
import { ofArray, map as map_1, delay, iterate } from "./.fable/fable-library.3.1.5/Seq.js";
import * as react from "react";
import { printf, toConsole } from "./.fable/fable-library.3.1.5/String.js";
import { ProgramModule_mkSimple, ProgramModule_withConsoleTrace, ProgramModule_run } from "./.fable/Fable.Elmish.4.0.0-alpha-1/program.fs.js";
import { Program_withReactSynchronous } from "./.fable/Fable.Elmish.React.4.0.0-alpha-1/react.fs.js";

export const count = createAtom(0);

export class Port extends Record {
    constructor(title, guid) {
        super();
        this.title = title;
        this.guid = guid;
    }
}

export function Port$reflection() {
    return record_type("App.Port", [], Port, () => [["title", string_type], ["guid", class_type("System.Guid")]]);
}

export class Node$ extends Record {
    constructor(title, guid, pos, inputs, outputs) {
        super();
        this.title = title;
        this.guid = guid;
        this.pos = pos;
        this.inputs = inputs;
        this.outputs = outputs;
    }
}

export function Node$$reflection() {
    return record_type("App.Node", [], Node$, () => [["title", string_type], ["guid", class_type("System.Guid")], ["pos", tuple_type(int32_type, int32_type)], ["inputs", list_type(Port$reflection())], ["outputs", list_type(Port$reflection())]]);
}

export class Edge extends Record {
    constructor(fromPort, toPort) {
        super();
        this.fromPort = fromPort;
        this.toPort = toPort;
    }
}

export function Edge$reflection() {
    return record_type("App.Edge", [], Edge, () => [["fromPort", class_type("System.Guid")], ["toPort", class_type("System.Guid")]]);
}

export class Graph extends Record {
    constructor(nodes, edges) {
        super();
        this.nodes = nodes;
        this.edges = edges;
    }
}

export function Graph$reflection() {
    return record_type("App.Graph", [], Graph, () => [["nodes", list_type(Node$$reflection())], ["edges", list_type(Edge$reflection())]]);
}

export function emptyGraph() {
    return new Graph(empty(), empty());
}

export function newPort(title) {
    const guid = newGuid();
    return new Port(guid, guid);
}

export function newNode() {
    const guid = newGuid();
    return new Node$(guid, guid, [0, 0], empty(), empty());
}

export class GraphBuilder {
    constructor() {
        this.g = emptyGraph();
    }
}

export function GraphBuilder$reflection() {
    return class_type("App.GraphBuilder", void 0, GraphBuilder);
}

export function GraphBuilder_$ctor() {
    return new GraphBuilder();
}

export function GraphBuilder__AddNode_Z99051AC(this$, title, pos, inputs, outputs) {
    const guid = newGuid();
    const n = new Node$(defaultArg(title, guid), guid, defaultArg(pos, [0, 0]), map((title_1) => newPort(title_1), defaultArg(inputs, empty())), map((title_2) => newPort(title_2), defaultArg(outputs, empty())));
    this$.g = (new Graph(cons(n, this$.g.nodes), this$.g.edges));
    return this$;
}

export function GraphBuilder__Build(this$) {
    return this$.g;
}

export const gb = GraphBuilder__AddNode_Z99051AC(GraphBuilder__AddNode_Z99051AC(GraphBuilder_$ctor(), "A", [1, 1]), "B", [20, 20]);

export const g = GraphBuilder__Build(gb);

export function render(g_1) {
    const w = 100;
    const h = 50;
    let b = fill(new Array(w * h), 0, w * h, "_");
    const set$ = (x, y, c) => {
        b[x + (y * w)] = c;
    };
    const renderNode = (n) => {
        const patternInput = [6, 4];
        const nw = patternInput[0] | 0;
        const nh = patternInput[1] | 0;
        const patternInput_1 = n.pos;
        const y_1 = patternInput_1[1] | 0;
        const x_1 = patternInput_1[0] | 0;
        for (let i = 0; i <= (nw - 1); i++) {
            for (let j = 0; j <= (nh - 1); j++) {
                set$(x_1 + i, y_1 + j, ((((i === 0) ? true : (i === (nw - 1))) ? true : (j === (nh - 1))) ? true : (j === 0)) ? "X" : ".");
            }
        }
    };
    iterate(renderNode, g_1.nodes);
    return delay(() => map_1((line) => react.createElement("span", {}, line.join('')), ofArray(chunkBySize(w, Array.from(b)))));
}

export class Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Increment", "Decrement"];
    }
}

export function Msg$reflection() {
    return union_type("App.Msg", [], Msg, () => [[], []]);
}

export function init() {
    return 0;
}

export function update(msg, model) {
    if (msg.tag === 1) {
        return (model - 1) | 0;
    }
    else {
        return (model + 1) | 0;
    }
}

export function view(model, dispatch) {
    return react.createElement("div", {}, react.createElement("p", {}, toString(g)), react.createElement("p", {
        className: "graph-output",
    }, ...render(g)), react.createElement("button", {
        onClick: (_arg1) => {
            dispatch(new Msg(0));
        },
    }, "+"), react.createElement("div", {}, int32ToString(model)), react.createElement("button", {
        onClick: (_arg2) => {
            dispatch(new Msg(1));
        },
    }, "-"));
}

toConsole(printf("asd"));

ProgramModule_run(ProgramModule_withConsoleTrace(Program_withReactSynchronous("elmish-app", ProgramModule_mkSimple(init, (msg, model) => update(msg, model), (model_1, dispatch) => view(model_1, dispatch)))));

