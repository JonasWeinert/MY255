import pandas as pd

snapshots = pd.read_table("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Studium/phd/DATA/CovidBeliefsBehaviorsNormsSurvey/SingleWaveData/snapshot.txt")

snapshots.to_csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Studium/phd/DATA/CovidBeliefsBehaviorsNormsSurvey/SingleWaveData/snapshot.csv", index=False)