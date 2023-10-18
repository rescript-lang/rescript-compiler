/* TypeScript file generated from Comments.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as CommentsBS__Es6Import from './Comments.bs';
const CommentsBS: any = CommentsBS__Es6Import;

// eslint-disable-next-line consistent-type-definitions
export type DecideSubject_payload = { 
/** A hint to use as a guide when thinking of your poem. */
readonly hint: string };

// eslint-disable-next-line max-classes-per-file naming-convention
/** The input used to generate the prompt and system prompt. */
export abstract class DecideSubject_input { protected opaque!: any }; /* simulate opaque types */

// eslint-disable-next-line consistent-type-definitions
/** The output from evaluating the llm prompt */
export type DecideSubject_output = {
  /** The text of the poem. */
  readonly text: string; 
  /** The prompt used to generate the poem. */
  readonly prompt: string; 
  /** The system prompt used to generate the poem. */
  readonly systemPrompt: string
};

/** Decide on a subject matter for a poem. */
export const DecideSubject__placeholder: (run:string, times:number) => void = CommentsBS.DecideSubject._placeholder;

export const DecideSubject: { 
/** Decide on a subject matter for a poem. */
_placeholder: (run:string, times:number) => void } = CommentsBS.DecideSubject
