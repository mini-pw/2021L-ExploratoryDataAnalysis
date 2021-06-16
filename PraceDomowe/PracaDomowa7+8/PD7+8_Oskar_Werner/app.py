import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

styles = {
    'pre': {
        'border': 'thin lightgrey solid',
        'overflowX': 'scroll'
    }
}
# kredytobiorcy
df = pd.read_csv("dane_dash.csv")
df = df.drop(columns=['Unnamed: 0'])

i = "HF16L"
dfFran = df \
    .loc[df['HF12'] == "WE FRANKACH SZWAJCARSKICH"] \
    .loc[pd.notna(df[i]), [i]] \
    .value_counts() \
    .reset_index() \
    .rename(columns={0: 'CountFran'}) \
    .dropna()

dfReszta = df \
    .loc[pd.notna(df[i]), [i]] \
    .loc[df['HF12'] != "WE FRANKACH SZWAJCARSKICH"] \
    .value_counts() \
    .reset_index() \
    .rename(columns={0: 'CountAll'}) \
    .dropna()

dfRazem = dfReszta \
    .merge(dfFran, how='left', on=i) \
    .fillna(value={'CountFran': 0, 'CountAll': 0}) \
    .sort_values(by=i)
dfRazem[['CountAll', 'CountFran']] = dfRazem[['CountAll', 'CountFran']] / [dfRazem.CountAll.sum(),
                                                                           dfRazem.CountFran.sum()]
dfRazem = pd.melt(dfRazem, value_vars=("CountAll", "CountFran"), id_vars="HF16L")

dfRazem.loc[dfRazem["variable"]=="CountAll","variable"]='Pozostali kredytobiorcy'
dfRazem.loc[dfRazem["variable"]=="CountFran","variable"]='Kredytobiorcy we frankach'
dfRazem["Odsetek respondentów"]=dfRazem["value"]
dfRazem["Czas w latach"]=dfRazem["HF16L"]
dfRazem["Rodzaj Kredytobiorcy"]=dfRazem["variable"]
# fig2
fig2 = px.bar(dfRazem, x="Czas w latach", y="Odsetek respondentów", color="Rodzaj Kredytobiorcy", barmode="group")

fig2.update_layout(clickmode='event+select')

fig2.update_layout(
    title={
        'text': "Ile czasu zastało do spłaty kredytu",
        'y': 0.9,
        'x': 0.5,
        'font': dict(
            size=20
        ),
        'xanchor': 'center',
        'yanchor': 'top'})
# fig2
i = "HF13"
dfFran = df \
    .loc[df['HF12'] == "WE FRANKACH SZWAJCARSKICH"] \
    .loc[pd.notna(df[i]), [i]] \
    .value_counts() \
    .reset_index() \
    .rename(columns={0: 'CountFran'}) \
    .dropna()

dfReszta = df \
    .loc[pd.notna(df[i]), [i]] \
    .loc[df['HF12'] != "WE FRANKACH SZWAJCARSKICH"] \
    .value_counts() \
    .reset_index() \
    .rename(columns={0: 'CountAll'}) \
    .dropna()

dfRazem = dfReszta \
    .merge(dfFran, how='left', on=i) \
    .fillna(value={'CountFran': 0, 'CountAll': 0}) \
    .sort_values(by=i)
dfRazem[['CountAll', 'CountFran']] = dfRazem[['CountAll', 'CountFran']] / [dfRazem.CountAll.sum(),
                                                                           dfRazem.CountFran.sum()]
dfRazem = pd.melt(dfRazem, value_vars=("CountAll", "CountFran"), id_vars="HF13")

dfRazem.loc[dfRazem["variable"]=="CountAll","variable"]='Pozostali kredytobiorcy'
dfRazem.loc[dfRazem["variable"]=="CountFran","variable"]='Kredytobiorcy we frankach'
dfRazem["Odsetek respondentów"]=dfRazem["value"]
dfRazem["Wkład Własny w procentach"]=dfRazem["HF13"]
dfRazem["Rodzaj Kredytobiorcy"]=dfRazem["variable"]
# fig3
fig3 = px.bar(dfRazem, x="Wkład Własny w procentach", y="Odsetek respondentów", color="Rodzaj Kredytobiorcy", barmode="group")

fig3.update_layout(clickmode='event+select')

fig3.update_layout(

    title={
        'text': "Jaką część ceny nieruchomości w momencie wzięcia kredytu stanowił wkład własny",
        'y': 0.9,
        'x': 0.5,
        'font': dict(
            size=20
        ),
        'xanchor': 'center',
        'yanchor': 'top'})

# frank
frank = pd.read_csv("KursFrank.csv")

fig = px.line(frank, x="Data", y="Kurs średni")

fig.update_layout(clickmode='event+select')

fig.update_layout(
    title={
        'text': "Kurs franka w czasie",
        'y': 0.9,
        'x': 0.5,
        'font': dict(
            size=20
        ),
        'xanchor': 'center',
        'yanchor': 'top'})

app.layout = html.Div([
    dcc.Graph(
        id='Frank',
        figure=fig
    ),
    dcc.Graph(
        id='HF16L',
        figure=fig2
    ),
    dcc.Graph(
        id='HF13',
        figure=fig3
    ),
    html.H3(children=["Główną zaletą interaktywności wykresów jest to że można dostać dokładne informacje z tooltip, oraz można przybliżać interesujące nas fragmenty wykresu"])
])

if __name__ == '__main__':
    app.run_server(debug=True)
