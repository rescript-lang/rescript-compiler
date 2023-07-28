/* TypeScript file generated from Comments.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as CommentsBS__Es6Import from './Comments.bs';
const CommentsBS: any = CommentsBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type DecideSubject_payload = { 
/** A hint to use as a guide when thinking of your poem.*/
readonly hint: string };

// tslint:disable-next-line:max-classes-per-file 
/** The input used to generate the prompt and system prompt. */
export abstract class DecideSubject_input { protected opaque!: any }; /* simulate opaque types */

// tslint:disable-next-line:interface-over-type-literal
/** The output from evaluating the llm prompt */
export type DecideSubject_output = {
  /** The text of the poem.*/
  readonly text: string; 
  /** The prompt used to generate the poem.*/
  readonly prompt: string; 
  /** The system prompt used to generate the poem.*/
  readonly systemPrompt: string
};

/** Decide on a subject matter for a poem. */
export const DecideSubject__placeholder: (run:string, times:number) => void = CommentsBS.DecideSubject._placeholder;

export const DecideSubject: { _placeholder: (run:string, times:number) => void } = CommentsBS.DecideSubject
