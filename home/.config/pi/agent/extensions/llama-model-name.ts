import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

const API_URL = "http://localhost:11434/v1/models";

// llama-server (OpenAI-compatible) returns { data: [{ id, ... }] }
type ModelList = { data?: { id?: string; aliases?: string[] }[] };

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "model_real_name",
    label: "Model Real Name",
    description:
      "Get the real upstream model name behind the current pi model. The pi model id may be a generic alias (e.g. 'agent'), so use this real name in git commit messages.",
    parameters: Type.Object({}),
    async execute(_toolCallId, _params, _signal, _onUpdate, ctx) {
      let res: Response;
      try {
        res = await fetch(API_URL);
      } catch (e) {
        return {
          content: [{ type: "text", text: `Cannot reach model server at ${API_URL}: ${(e as Error).message}` }],
          isError: true,
        };
      }

      const body = (await res.json()) as ModelList;
      const models = body.data ?? [];

      const currentId = ctx.model?.id;
      const match =
        models.find((m) => m.id === currentId || m.aliases?.includes(currentId)) ??
        (models.length === 1 ? models[0] : undefined);

      if (!match?.id) {
        return {
          content: [{ type: "text", text: `No matching model on server` }],
          isError: true,
        };
      }

      return { content: [{ type: "text", text: match.id }], details: { id: match.id } };
    },
  });
}
