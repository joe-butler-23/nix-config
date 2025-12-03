#!/usr/bin/env npx tsx

import fs from 'fs';
import path from 'path';

interface SkillRule {
  name: string;
  priority: 'critical' | 'high' | 'medium' | 'low';
  triggers: {
    keywords: string[];
    intents: string[];
  };
}

interface SkillRules {
  skills: SkillRule[];
}

interface MatchedSkill {
  name: string;
  priority: string;
  matchType: 'keyword' | 'intent';
  matchValue: string;
}

class SkillActivationPrompt {
  private skillRules: SkillRules;
  private projectDir: string;

  constructor() {
    this.projectDir = process.env.CLAUDE_PROJECT_DIR || process.cwd();
    this.loadSkillRules();
  }

  private loadSkillRules(): void {
    const rulesPath = path.join(this.projectDir, '.claude', 'hooks', 'skill-rules.json');

    try {
      const rulesContent = fs.readFileSync(rulesPath, 'utf8');
      this.skillRules = JSON.parse(rulesContent);
    } catch (error) {
      console.error(`Error loading skill rules from ${rulesPath}:`, error);
      process.exit(1);
    }
  }

  private matchKeywords(prompt: string, keywords: string[]): string[] {
    const promptLower = prompt.toLowerCase();
    return keywords.filter(keyword =>
      promptLower.includes(keyword.toLowerCase())
    );
  }

  private matchIntents(prompt: string, intents: string[]): string[] {
    return intents.filter(intent => {
      try {
        const regex = new RegExp(intent, 'i');
        return regex.test(prompt);
      } catch (error) {
        console.error(`Invalid regex pattern: ${intent}`, error);
        return false;
      }
    });
  }

  private analyzePrompt(prompt: string): MatchedSkill[] {
    const matches: MatchedSkill[] = [];

    for (const skill of this.skillRules.skills) {
      // Check keyword matches
      const keywordMatches = this.matchKeywords(prompt, skill.triggers.keywords);
      for (const match of keywordMatches) {
        matches.push({
          name: skill.name,
          priority: skill.priority,
          matchType: 'keyword',
          matchValue: match
        });
      }

      // Check intent matches
      const intentMatches = this.matchIntents(prompt, skill.triggers.intents);
      for (const match of intentMatches) {
        matches.push({
          name: skill.name,
          priority: skill.priority,
          matchType: 'intent',
          matchValue: match
        });
      }
    }

    // Remove duplicates by skill name
    const uniqueMatches = matches.filter((match, index, arr) =>
      arr.findIndex(m => m.name === match.name) === index
    );

    return uniqueMatches;
  }

  private groupByPriority(matches: MatchedSkill[]): Record<string, MatchedSkill[]> {
    const groups: Record<string, MatchedSkill[]> = {
      critical: [],
      high: [],
      medium: [],
      low: []
    };

    for (const match of matches) {
      groups[match.priority].push(match);
    }

    return groups;
  }

  private formatOutput(matches: MatchedSkill[]): string {
    if (matches.length === 0) {
      return '';
    }

    const groups = this.groupByPriority(matches);
    let output = '\n';
    output += '━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n';
    output += '🎯 SKILL ACTIVATION ANALYSIS\n';
    output += '━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n';

    const priorityOrder = ['critical', 'high', 'medium', 'low'];
    const priorityEmojis = {
      critical: '🔴',
      high: '🟠',
      medium: '🟡',
      low: '🔵'
    };

    let hasMatches = false;

    for (const priority of priorityOrder) {
      const skillsInPriority = groups[priority];
      if (skillsInPriority.length > 0) {
        hasMatches = true;
        output += `${priorityEmojis[priority]} **${priority.toUpperCase()} PRIORITY SKILLS:**\n`;

        for (const skill of skillsInPriority) {
          output += `   • ${skill.name} (matched: "${skill.matchValue}")\n`;
        }
        output += '\n';
      }
    }

    if (hasMatches) {
      output += '💡 **ACTION REQUIRED**: Use the Skill tool to activate relevant skills BEFORE responding.\n';
      output += '   Example: Use Skill tool with skill name "beads-workflow"\n\n';

      // Add skill names for easy reference
      output += '📋 **MATCHED SKILLS**: ';
      const skillNames = [...new Set(matches.map(m => m.name))];
      output += skillNames.join(', ') + '\n';
    }

    return output;
  }

  public run(): void {
    try {
      // Read input from stdin
      let input = '';
      process.stdin.setEncoding('utf8');

      if (process.stdin.isTTY) {
        // If running interactively, read from command line args
        input = process.argv.slice(2).join(' ');
      } else {
        // Read from stdin (pipe)
        const chunks: string[] = [];
        process.stdin.on('readable', () => {
          const chunk = process.stdin.read();
          if (chunk !== null) {
            chunks.push(chunk);
          }
        });

        process.stdin.on('end', () => {
          input = chunks.join('');
          this.processInput(input.trim());
        });
        return;
      }

      this.processInput(input);
    } catch (error) {
      console.error('Error in skill activation prompt:', error);
      process.exit(1);
    }
  }

  private processInput(input: string): void {
    if (!input) {
      process.exit(0);
    }

    const matches = this.analyzePrompt(input);
    const output = this.formatOutput(matches);

    if (output) {
      console.log(output);
    }

    process.exit(0);
  }
}

// Run the analysis
const analyzer = new SkillActivationPrompt();
analyzer.run();
