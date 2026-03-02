import pandas as pd

df = pd.read_csv("./data cleaning/cleaned_mlbdraft.csv")

# normalize Signed to boolean
def norm_signed(x):
    if pd.isna(x): 
        return pd.NA
    s = str(x).strip().lower()
    if s in {"signed","yes","y","true","t","1"}:
        return True
    if s in {"did not sign","no","n","false","f","0","unsigned"}:
        return False
    return pd.NA

df["Signed_bool"] = df["Signed"].apply(norm_signed).astype("boolean")

# ensure Bonus is numeric (assumes your Bonus is in millions already; adjust if dollars)
df["Bonus_num"] = pd.to_numeric(df["Bonus"], errors="coerce")
df["Bonus_gt0"] = df["Bonus_num"] > 0

# drop rows where Signed is missing
sub = df[df["Signed_bool"].notna() & df["Bonus_num"].notna()].copy()

ct = pd.crosstab(sub["Signed_bool"], sub["Bonus_gt0"], rownames=["Signed"], colnames=["Bonus>0"])
print(ct)

accuracy = (sub["Signed_bool"] == sub["Bonus_gt0"]).mean()
print("Agreement rate (Signed == (Bonus>0)):", round(accuracy, 4))

import pandas as pd

df = pd.read_csv("./data cleaning/cleaned_mlbdraft.csv")

def norm_signed(x):
    if pd.isna(x): 
        return pd.NA
    s = str(x).strip().lower()
    if s in {"signed","yes","y","true","t","1"}:
        return True
    if s in {"did not sign","no","n","false","f","0","unsigned","didnt sign","didn't sign"}:
        return False
    return pd.NA

df["Signed_bool"] = df["Signed"].apply(norm_signed).astype("boolean")
df["Bonus_num"] = pd.to_numeric(df["Bonus"], errors="coerce")
df["Bonus_gt0"] = df["Bonus_num"] > 0

sub = df[df["Signed_bool"].notna() & df["Bonus_num"].notna()].copy()

# overall metrics
tp = ((sub["Signed_bool"] == True)  & (sub["Bonus_gt0"] == True)).sum()
tn = ((sub["Signed_bool"] == False) & (sub["Bonus_gt0"] == False)).sum()
fp = ((sub["Signed_bool"] == False) & (sub["Bonus_gt0"] == True)).sum()   # bonus>0 but says not signed
fn = ((sub["Signed_bool"] == True)  & (sub["Bonus_gt0"] == False)).sum()  # signed but bonus==0

print({"TP": tp, "TN": tn, "FP": fp, "FN": fn, "N": len(sub)})
print("Agreement:", round((tp+tn)/len(sub), 4))

# show the exceptions
exceptions = sub[(sub["Signed_bool"] != sub["Bonus_gt0"])].copy()
print("\nExceptions (first 30):")
cols = [c for c in ["Year","Rnd","OvPck","Name","Bonus","Signed"] if c in exceptions.columns]
print(exceptions[cols].head(30))

# by-year agreement (useful to see if the rule holds more strongly in some eras)
if "Year" in sub.columns:
    by_year = sub.groupby("Year").apply(lambda g: (g["Signed_bool"] == g["Bonus_gt0"]).mean())
    print("\nAgreement by Year:")
    print(by_year.sort_index().round(4))