import React from "react";
import { Container, Grid, Button, TextField } from "@material-ui/core";
import { useStyles } from "./Style";
import { Title } from "./Common";
import { setAuth } from "./Api";

interface LoginScreenProps {}

export const LoginScreen: React.FunctionComponent<LoginScreenProps> = (
  props
) => {
  const classes = useStyles();

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item>
          <Title>Log in</Title>
        </Grid>

        <Grid item xs={12}>
          <LoginForm />
        </Grid>
      </Grid>
    </Container>
  );
};

interface LoginFormProps {}

interface FormInputData {
  username: string;
  password: string;
}

const initialFormInputData = {
  username: "",
  password: "",
};

export const LoginForm: React.FunctionComponent<LoginFormProps> = (props) => {
  const [formData, setFormData] = React.useState<FormInputData>(
    initialFormInputData
  );

  const onSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    // Don't automatically reload on submit.
    event.preventDefault();
    setAuth(formData.username, formData.password);
  };

  return (
    <form onSubmit={onSubmit}>
      <Grid
        container
        direction="column"
        justify="flex-start"
        alignItems="flex-start"
        spacing={3}
      >
        <Grid item xs={12}>
          <TextField
            id="login-username"
            label="Username"
            variant="outlined"
            value={formData.username}
            onChange={(event: React.ChangeEvent<HTMLInputElement>) => {
              setFormData({
                ...formData,
                username: event.target.value,
              });
            }}
          />
        </Grid>

        <Grid item xs={12}>
          <TextField
            id="login-password"
            label="Password"
            variant="outlined"
            value={formData.password}
            onChange={(event: React.ChangeEvent<HTMLInputElement>) => {
              setFormData({
                ...formData,
                password: event.target.value,
              });
            }}
          />
        </Grid>

        <Grid item xs={12}>
          <Button variant="contained" color="primary" type="submit">
            Save
          </Button>
        </Grid>
      </Grid>
    </form>
  );
};
