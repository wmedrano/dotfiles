/**
 * otto - Functionality for directing a chat session and its scratch workspace.
 */

import { readdir } from "node:fs/promises";
import { join, resolve } from "node:path";
import {
  type AgentEndEvent, type BeforeAgentStartEvent, type ExtensionAPI,
  type ExtensionContext, type ToolResultEvent, type SessionStartEvent
} from "@earendil-works/pi-coding-agent";
import { Key } from "@earendil-works/pi-tui";
import { Scratchy } from "./scratch.js";
import { executePlanPrompt, scratchSystemPrompt, reportPrompt, planPrompt } from "./prompts.js";

enum PlanStatus { None, InProgress, Drafted, Executing };
type Mode = "auto" | "normal";

const MODE_INFO: Record<Mode, { icon: string; label: string }> = {
  auto: { icon: "🐇", label: "auto" },
  normal: { icon: "🐢", label: "normal" },
};

export default function otto(pi: ExtensionAPI) {
  let scratchy: Scratchy;
  let showScratchStatus = false;
  let planPath = "";
  let planStatus = PlanStatus.None;
  let mode: Mode = "normal";

  pi.registerShortcut(Key.alt("m"), {
    description: "Toggle otto mode (auto/normal)",
    handler: toggleMode,
  });

  function updateStatus(ctx: ExtensionContext) {
    const theme = ctx.ui.theme;
    let parts = [];

    // Mode
    const modeInfo = MODE_INFO[mode];
    parts.push(ctx.ui.theme.fg("muted", `${modeInfo.icon} ${modeInfo.label}`));

    // Plan
    const link = scratchy.link(planPath);
    switch (planStatus) {
      case PlanStatus.None:
        break;
      case PlanStatus.InProgress:
        parts.push(theme.fg("muted", "📝 planning…"));
        break;
      case PlanStatus.Drafted:
        parts.push(theme.fg("accent", `📝 ${link}`));
        break;
      case PlanStatus.Executing:
        parts.push(theme.fg("accent", `🚀 ${link}`));
    }

    // Scratch dir
    if (showScratchStatus) {
      parts.push(theme.fg("muted", scratchy.link("")));
    }

    const separator = theme.fg("dim", "  |  ");
    ctx.ui.setStatus("otto", parts.join(separator));
  }

  function setMode(newMode: Mode, ctx: ExtensionContext) {
    mode = newMode;
    const { icon, label } = MODE_INFO[mode];
    ctx.ui.notify(`${icon} Mode: ${label}`, "info");
    updateStatus(ctx);
  }

  function toggleMode(ctx: ExtensionContext) {
    setMode(mode === "auto" ? "normal" : "auto", ctx);
  }

  function startPlan(ctx: ExtensionContext): string {
    planPath = scratchy.reserve("plan", "md");
    planStatus = PlanStatus.InProgress;
    updateStatus(ctx);
    pi.appendEntry("otto.planStart", planPath);
    return planPath;
  }

  async function xdgOpen(file: string): Promise<boolean> {
    const { code } = await pi.exec("xdg-open", [file], { timeout: 1000 });
    // pi.exec resolves with a non-zero code instead of rejecting when the binary is missing.
    return code === 0;
  }

  pi.on("tool_result", onToolResult);
  function onToolResult(event: ToolResultEvent, ctx: ExtensionContext) {
    // Nested calls (e.g. from codemode scripts) re-emit tool_result; only report
    // model-issued calls, so notifications don't duplicate.
    if (event.parentToolCallId !== undefined) { return; }
    if (event.isError) { return; }
    if (event.toolName !== "edit" && event.toolName !== "write") { return; }
    if (typeof event.input.path !== "string") { return; }
    if (!scratchy.contains(event.input.path)) { return; }

    let statusChanged = false;
    const path = event.input.path;
    const isPlan = planPath !== "" && resolve(ctx.cwd, path) === resolve(planPath);
    if (isPlan) {
      if (planStatus === PlanStatus.InProgress) {
        planStatus = PlanStatus.Drafted;
        statusChanged = true;
      }
    } else {
      showScratchStatus = true;
      statusChanged = true;
    }
    const verb = event.toolName === "edit" ? "Updated" : "Wrote";
    ctx.ui.notify(`🕵️ ${verb} scratch file ${scratchy.link(path)}`, "info");
    if (statusChanged) {
      updateStatus(ctx);
    }
  }

  pi.on("agent_end", onAgentEnd);
  function onAgentEnd(_event: AgentEndEvent, ctx: ExtensionContext) {
    if (planStatus === PlanStatus.Executing) {
      // The run that executed the plan just finished; reset the lifecycle.
      planStatus = PlanStatus.None;
      updateStatus(ctx);
      return;
    }
    if (mode !== "auto" || planStatus !== PlanStatus.Drafted) { return; }
    planStatus = PlanStatus.Executing;
    updateStatus(ctx);
    const link = scratchy.link(planPath);
    ctx.ui.notify(`${MODE_INFO["auto"].icon} Plan ${link} drafted — auto-approving`, "info");
    pi.sendUserMessage(executePlanPrompt(), { deliverAs: "followUp" });
  }

  pi.on("session_start", onSessionStart);
  function onSessionStart(_: SessionStartEvent, ctx: ExtensionContext) {
    scratchy = new Scratchy(ctx.cwd);
    updateStatus(ctx);
  }

  pi.on("before_agent_start", onBeforeAgentStart);
  async function onBeforeAgentStart(event: BeforeAgentStartEvent, _: ExtensionContext) {
    const scratch = await scratchy.ensureScratchDir();
    event.systemPromptOptions.sections["scratch"] = scratchSystemPrompt(scratch);
  }

  pi.registerCommand("queue", {
    description: `Inject a message to be sent after the current run.`,
    handler: onCommandQueue
  })
  async function onCommandQueue(args: string, ctx: ExtensionContext) {
    const text = args.trim();
    if (text === "") {
      ctx.ui.notify(`⚠️ No message was provided to /queue`, "warning");
      return;
    }
    pi.sendUserMessage(text, { deliverAs: "followUp" });
  }

  pi.registerCommand("mode", {
    description: `Switch otto mode: auto (auto-approve drafted plans) or normal. Toggles when no argument is given.`,
    handler: onCommandMode,
  });
  async function onCommandMode(args: string, ctx: ExtensionContext) {
    const arg = args.trim().toLowerCase();
    if (arg === "") {
      toggleMode(ctx);
      return;
    }
    if (arg === "auto" || arg === "normal") {
      setMode(arg as Mode, ctx);
      return;
    }
    ctx.ui.notify(`⚠️ Unknown mode "${arg}". Expected "auto" or "normal".`, "warning");
  }

  pi.registerCommand("scratch", {
    description: "Open the session scratch directory",
    handler: onCommandScratch,
  });
  async function onCommandScratch(_args: string, ctx: ExtensionContext) {
    const dir = await scratchy.ensureScratchDir();
    const ok = await xdgOpen(dir);
    const notification = ok ? `Opened ${dir}` : `Scratch: ${dir}`;
    ctx.ui.notify(notification, "info");
  }

  pi.registerCommand("artifact", {
    description: "Open an artifact.",
    handler: onCommandArtifact,
  });
  async function onCommandArtifact(_args: string, ctx: ExtensionContext) {
    const dir = await scratchy.ensureScratchDir();
    const entries = await readdir(dir, { withFileTypes: true, recursive: true });
    // With recursive readdir, entry.name is only the base name, so rebuild the
    // full path from the directory the entry was read from.
    const files = entries
      .filter((entry) => entry.isFile())
      .map((entry) => join(entry.parentPath, entry.name));
    if (files.length === 0) {
      ctx.ui.notify(`No files in ${dir}`, "warning");
      return;
    }
    const file = await ctx.ui.select("Open artifact", files);
    if (file) {
      await xdgOpen(file);
    }
  }

  pi.registerCommand("autoplan", {
    description: `Switch to auto mode and create a plan (drafted plans are auto-approved).`,
    handler: onCommandAutoplan,
  });
  async function onCommandAutoplan(args: string, ctx: ExtensionContext) {
    // Set the mode first so the auto-approve path in onAgentEnd fires when the
    // planning run finishes.
    setMode("auto", ctx);
    await onCommandPlan(args, ctx);
  }

  pi.registerCommand("plan", {
    description: "Create a plan.",
    handler: onCommandPlan,
  });
  async function onCommandPlan(args: string, ctx: ExtensionContext) {
    const path = startPlan(ctx);
    const prompt = planPrompt(path, args.trim());
    pi.sendUserMessage(prompt, { deliverAs: "steer" });
  }

  pi.registerCommand("report", {
    description: "Create a report.",
    handler: onCommandReport,
  });
  async function onCommandReport(args: string, ctx: ExtensionContext) {
    const path = scratchy.reserve("report", "md");
    const prompt = reportPrompt(path, args.trim());
    pi.sendUserMessage(prompt, { deliverAs: "followUp" });
  }
}
