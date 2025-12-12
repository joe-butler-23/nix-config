---
name: self-learning
description: AI continuous improvement system that detects learning opportunities and evolves skills based on usage patterns. Use when experiencing repeated patterns, skill gaps, workflow inefficiencies, error recovery situations, or when existing skills have not worked as expected and need improvement. Triggers on: iterative refinements, skill modifications, new domain requests, troubleshooting patterns, or workflow optimization opportunities.
---

# Self-Learning Skill

Continuous AI improvement through learning opportunity detection and skill evolution. You should stay vigilant for opportunities to make improvements to either the process you are using in conjunction with the user, or to the domain-specific knowledge you have been provided. You are a key player in a process of continuous self-improvement. Do not make the same mistake twice: document and learn from your mistakes.

## Core Learning Workflows

### 1. Learning Opportunity Detection

Recognize these patterns that indicate learning opportunities:

**High Priority (immediate action)**:
- Repeated skill file modifications
- Same troubleshooting pattern across sessions
- Error recovery requiring manual intervention
- Workflow inefficiencies causing multiple iterations
- Errors caused by inaccurate information in the provided context

**Medium Priority (session review)**:
- New domain requests without existing skills
- Repeated tool usage patterns
- Frequently asked questions
- Workflow optimization opportunities

**Low Priority (future consideration)**:
- General pattern observations
- Minor efficiency improvements
- Edge case handling

### 2. Learning Analysis Process

When learning opportunity detected:

1. **Analyze Context**
   - What triggered the learning opportunity?
   - What domain/workflow is involved?
   - Is this a new skill need or existing skill improvement?
   - What's the impact/frequency of the pattern?

2. **Determine Action Type**
   - **Skill Creation**: New domain requiring dedicated skill
   - **Skill Enhancement**: Existing skill needs improvement
   - **Workflow Optimization**: Process improvement opportunity
   - **Documentation Gap**: Missing guidance or examples

3. **Generate Improvement Plan**
   - Specific changes needed
   - Resources required (references, scripts, assets)
   - Implementation approach
   - Success criteria

### 3. Skill Evolution Workflows

**For New Skills**:
- Use skill-creator skill with concrete examples
- Follow progressive disclosure patterns
- Include appropriate bundled resources
- Test with real scenarios

**For Existing Skill Improvements**:
- Identify specific enhancement needs
- Update SKILL.md with new patterns/workflows
- Add missing reference materials
- Update scripts/assets as needed

**For Workflow Optimizations**:
- Document improved patterns
- Create scripts for repeated tasks
- Update relevant skill guidance
- Share learnings across related skills

### 4. Learning Implementation

**Immediate Implementation** (high priority):
- Apply skill improvements directly
- Document new patterns in existing skills
- Create quick reference materials

**Planned Implementation** (medium/low priority):
- File as Org tasks for structured tracking
- Plan implementation in future sessions
- Gather additional examples before acting

### 5. Quality Assurance (The "Lesson" vs "Log" Test)

Before committing any learning update, ask:
- **Is this descriptive?** ("I failed to install X because Y option was missing") -> **REJECT**. This is just a log.
- **Is this predictive?** ("Always verify options for hardware tools in authoritative docs before using") -> **ACCEPT**. This prevents future errors.
- **Is this generalized?** Does it apply to other similar tools/contexts, or just this specific package?
- **Does it change behavior?** Will reading this actually alter how the agent approaches the next task?

**Rule:** We do not want a history of *what happened*. We want a playbook of *how to act better next time*.
