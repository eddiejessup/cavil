import React from "react";
import { Typography } from "@material-ui/core";
import { parseISO, format } from "date-fns";

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

export type ErrorMsg = string;

export interface FetchSuccess<V> {
  kind: "fetchSuccess";
  value: V;
}

export interface FetchError {
  kind: "fetchError";
  errMsg: ErrorMsg;
}

export interface NotFetched {
  kind: "notFetched";
}

export type FetchState<V> = NotFetched | FetchSuccess<V> | FetchError;

export const notFetched: <V>() => FetchState<V> = () => ({
  kind: "notFetched",
});

export const fetchError: <V>(errMsg: ErrorMsg) => FetchState<V> = (
  errMsg: ErrorMsg
) => ({ kind: "fetchError", errMsg });

export const fetchSuccess: <V>(value: V) => FetchState<V> = (value) => ({
  kind: "fetchSuccess",
  value,
});

export const formatISOString: (t: string) => string = (t) => {
  const dt = parseISO(t);
  return format(dt, "eee yyyy/MM/dd HH:mm OOO");
};
