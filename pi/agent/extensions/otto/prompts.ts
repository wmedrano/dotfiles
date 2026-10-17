export function planPrompt(path: string, prompt: string): string {
  const base = `# Instructions

Create a plan for the task and write it to ${path}

- Do not execute the plan. If experiments are needed, use the scratch directory.
- The plan must contain the following sections: [Goal, Steps]
- Work on the plan once the user gives explicit approval.`;
  if (prompt === "") return base;
  return `${base}\n\n# Task\n\n${prompt}`;
}

export function reportPrompt(path: string, prompt: string): string {
  if (prompt === "") {
    return `Create a report on your findings and write it to ${path}`;
  }
  return `# Instructions

Create a report and write it to ${path}

# Topic

${prompt}`;
}

export function scratchSystemPrompt(dir: string): string {
  return [
    `Scratch directory: ${dir}`,
    "- The scratch directory may be shared, prefer unique names for files to avoid clobbering.",
    "- Use it for temporary files: drafts, reports, intermediate output, throwaway logs, one-off scripts.",
    "- It already exists; make subdirectories with mkdir -p as needed.",
    "- Common use case: Store markdown files for requested plans, reviews, or reports."
  ].join("\n");
}

export function executePlanPrompt(): string {
  return "Implement the plan if it is ready. If not, ask me clarifying questions.";
}
