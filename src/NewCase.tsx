import React from "react";
import { Grid, TextField, Button } from "@material-ui/core";
import { CaseLabel } from "./Api";
import { Alert } from "@material-ui/lab";
import {cavilFetch} from "./Common"

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

  const onSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    // Don't automatically reload on submit.
    event.preventDefault()
    if (formData.nrVariants === null) {
      setFormError("Please enter a number of variants")
    } else {
      await cavilFetch({
        url: `/case/${formData.label}`,
        payload: { nrVariants: formData.nrVariants },
        fetchOpts: {
          method: "PUT",
        },
        onNiceError: (_, errObj) => {
          setFormError(`${errObj.errorType}: ${errObj.errorDetail}`)
        },
        on200: () => {
          window.location.reload()
        },
        onUglyError: (err) => {
          setFormError("Something went wrong")
        },
      })
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
