import React from "react";
import { Typography } from "@material-ui/core";
import { ClientError } from "./Api";

export const fallBackErrorMsg = "Something went wrong"

export const Title: React.FunctionComponent<{}> = (props) => (
  <Typography component="h2" variant="h5" color="primary" gutterBottom>
    {props.children}
  </Typography>
);

export const SubTitle: React.FunctionComponent<{}> = (props) => (
  <Typography component="h3" variant="h6" color="primary" gutterBottom>
    {props.children}
  </Typography>
);

interface CavilFetch {
  url: string;
  payload: any;
  fetchOpts: any;
  onNiceError: (status: number, err: ClientError) => void;
  on200: (res: any) => void;
  onUglyError: (err: Error) => void;
}

export const cavilFetch = async (args: CavilFetch) => {
  try {
    const res = await fetch(args.url, {
      ...args.fetchOpts,
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(args.payload),
    });
    if (res.status === 400) {
      const errObj = await res.json();
      args.onNiceError(res.status, errObj)
    } else if (res.status === 200) {
      console.log('got 200')
      console.log('got resp:')
      console.log(res)
      console.log('on 200:')
      args.on200(res);
      console.log('after on-200:')
    } else {
      throw new Error("Unknown error");
    }
  } catch (error) {
    // Try to decode error as ClientError, and show it like that. Otherwise
    // show a fallback error message.
    try {
      const errObj = await error.json();
      args.onNiceError(error.status, errObj);
    } catch (error) {
      args.onUglyError(error);
    }
  }
}
