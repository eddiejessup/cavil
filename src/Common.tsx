import React from "react";
import {
  Typography,
} from "@material-ui/core";

export const Title: React.FunctionComponent<{}> = (props) => (
  <Typography component="h2" variant="h6" color="primary" gutterBottom>
    {props.children}
  </Typography>
);
