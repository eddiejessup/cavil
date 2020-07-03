import React from "react";
import { Grid, TextField, Button } from "@material-ui/core";
import { CaseLabel } from "./Api";
import { Alert } from "@material-ui/lab";

interface FormInputData {
  label: CaseLabel;
  nrVariants: number | null;
}

const initialFormInputData = {
  label: "",
  nrVariants: 2,
};

interface NewCaseFormProps {}

export const NewCaseForm: React.FunctionComponent<NewCaseFormProps> = (
  props
) => {
  const [formError, setFormError] = React.useState<string | null>(null);
  const [formData, setFormData] = React.useState<FormInputData>(
    initialFormInputData
  );
  const [nrVariantsError, setNrVariantsError] = React.useState<boolean>(false);

  const onSubmit = async () => {
    if (formData.nrVariants != null) {
      try {
        const res = await fetch(`/case/${formData.label}`, {
          method: "PUT",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({ nrVariants: formData.nrVariants }),
        });
        if (res.status === 400) {
          const errObj = await res.json();
          console.log(res.status);
          setFormError(`${errObj.errorType}: ${errObj.errorDetail}`);
        } else if (res.status !== 200) {
          throw new Error("Unknown error");
        }
      } catch (error) {
        // Try to decode error as ClientError, and show it like that. Otherwise
        // show a fallback error message.
        try {
          const errObj = await error.json();
          setFormError(`${errObj.errorType}: ${errObj.errorDetail}`);
        } catch (error) {
          setFormError("Something went wrong");
        }
      }
    }
  };

  return (
    <form onSubmit={onSubmit}>
      <Grid
        container
        direction="row"
        justify="flex-start"
        alignItems="flex-start"
        spacing={3}
      >
        <Grid item xs={12}>
          <TextField
            id="case-label"
            label="Label"
            variant="outlined"
            required
            value={formData.label}
            onChange={(event: React.ChangeEvent<HTMLInputElement>) => {
              setFormData({
                ...formData,
                label: event.target.value as CaseLabel,
              });
            }}
          />
        </Grid>

        <Grid item xs={12}>
          <TextField
            error={nrVariantsError}
            helperText="Should be at least 2"
            id="case-nr-variants"
            label="Number of variants"
            variant="outlined"
            type="number"
            required
            value={formData.nrVariants || ""}
            onChange={(event: React.ChangeEvent<HTMLInputElement>) => {
              const nrVariants = parseInt(event.target.value);
              setNrVariantsError(isNaN(nrVariants) || nrVariants < 2);
              setFormData({
                ...formData,
                nrVariants: isNaN(nrVariants) ? null : nrVariants,
              });
            }}
          />
        </Grid>

        {formError && (
          <Grid item xs={12}>
            <Alert severity="error">{formError}</Alert>
          </Grid>
        )}

        <Grid item xs={12}>
          <Button
            variant="contained"
            color="primary"
            type="submit"
            disabled={
              nrVariantsError ||
              formData.nrVariants === null ||
              formData.label === ""
            }
          >
            Create case
          </Button>
        </Grid>
      </Grid>
    </form>
  );
};
