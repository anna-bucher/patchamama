/*
** reaper_csurf
** Copyright (C) 2006-2008 Cockos Incorporated
** License: LGPL.
*/
#include "reaper_stubs.h"

REAPER_PLUGIN_HINSTANCE g_hInst; // used for dialogs, if any
HWND g_hwnd;

double (*DB2SLIDER) (double x);
double (*SLIDER2DB) (double y);
int (*GetNumMIDIInputs) ();
int (*GetNumMIDIOutputs) ();
midi_Input *(*CreateMIDIInput) (int dev);
midi_Output *(*CreateMIDIOutput) (int dev, bool streamMode, int *msoffset100);
bool (*GetMIDIOutputName) (int dev, char *nameout, int nameoutlen);
bool (*GetMIDIInputName) (int dev, char *nameout, int nameoutlen);

void *(*projectconfig_var_addr) (void *proj, int idx);

void (*update_disk_counters) (int read, int write);

int (*CSurf_TrackToID) (MediaTrack *track, bool mcpView);
MediaTrack *(*CSurf_TrackFromID) (int idx, bool mcpView);
int (*CSurf_NumTracks) (bool mcpView);

// these will be called from app when something changes
void (*CSurf_SetTrackListChange) ();
void (*CSurf_SetSurfaceVolume) (MediaTrack *trackid, double volume,
                                IReaperControlSurface *ignoresurf);
void (*CSurf_SetSurfacePan) (MediaTrack *trackid, double pan,
                             IReaperControlSurface *ignoresurf);
void (*CSurf_SetSurfaceMute) (MediaTrack *trackid, bool mute,
                              IReaperControlSurface *ignoresurf);
void (*CSurf_SetSurfaceSelected) (MediaTrack *trackid, bool selected,
                                  IReaperControlSurface *ignoresurf);
void (*CSurf_SetSurfaceSolo) (MediaTrack *trackid, bool solo,
                              IReaperControlSurface *ignoresurf);
void (*CSurf_SetSurfaceRecArm) (MediaTrack *trackid, bool recarm,
                                IReaperControlSurface *ignoresurf);
bool (*CSurf_GetTouchState) (MediaTrack *trackid, int isPan);
void (*CSurf_SetAutoMode) (int mode, IReaperControlSurface *ignoresurf);

void (*CSurf_SetPlayState) (bool play, bool pause, bool rec,
                            IReaperControlSurface *ignoresurf);
void (*CSurf_SetRepeatState) (bool rep, IReaperControlSurface *ignoresurf);

// these are called by our surfaces, and actually update the project
double (*CSurf_OnVolumeChange) (MediaTrack *trackid, double volume,
                                bool relative);
double (*CSurf_OnPanChange) (MediaTrack *trackid, double pan, bool relative);
double (*CSurf_OnWidthChange) (MediaTrack *trackid, double width,
                               bool relative);
double (*CSurf_OnVolumeChangeEx) (MediaTrack *trackid, double volume,
                                  bool relative, bool allowgang);
double (*CSurf_OnPanChangeEx) (MediaTrack *trackid, double pan, bool relative,
                               bool allowgang);
double (*CSurf_OnWidthChangeEx) (MediaTrack *trackid, double width,
                                 bool relative, bool allowgang);
bool (*CSurf_OnMuteChange) (MediaTrack *trackid, int mute);
bool (*CSurf_OnMuteChangeEx) (MediaTrack *trackid, int mute, bool allowgang);
bool (*CSurf_OnSelectedChange) (MediaTrack *trackid, int selected);
bool (*CSurf_OnSoloChange) (MediaTrack *trackid, int solo);
bool (*CSurf_OnSoloChangeEx) (MediaTrack *trackid, int solo, bool allowgang);
bool (*CSurf_OnFXChange) (MediaTrack *trackid, int en);
bool (*CSurf_OnRecArmChange) (MediaTrack *trackid, int recarm);
bool (*CSurf_OnRecArmChangeEx) (MediaTrack *trackid, int recarm,
                                bool allowgang);
int (*CSurf_OnInputMonitorChange) (MediaTrack *trackid, int monitor);
int (*CSurf_OnInputMonitorChangeEx) (MediaTrack *trackid, int monitor,
                                     bool allowgang);
void (*CSurf_OnPlay) ();
void (*CSurf_OnStop) ();
void (*CSurf_OnPause) ();
void (*CSurf_OnRewFwd) (int seekplay, int amt);
void (*CSurf_OnRecord) ();
void (*CSurf_GoStart) ();
void (*CSurf_GoEnd) ();
void (*CSurf_OnArrow) (int whichdir, bool wantzoom);
void (*CSurf_OnScroll) (int xdir, int ydir);
void (*CSurf_OnZoom) (int xdir, int ydir);
void (*CSurf_OnTrackSelection) (MediaTrack *trackid);
void (*CSurf_ResetAllCachedVolPanStates) ();
void (*CSurf_ScrubAmt) (double amt);

void (*VkbStuffMessage) (MIDI_event_t *evt, bool wantCurChan);

double (*CSurf_OnSendVolumeChange) (MediaTrack *trackid, int sendidx,
                                    double volume, bool relative);
double (*CSurf_OnSendPanChange) (MediaTrack *trackid, int sendidx, double pan,
                                 bool relative);
double (*CSurf_OnRecvVolumeChange) (MediaTrack *trackid, int recvidx,
                                    double volume, bool relative);
double (*CSurf_OnRecvPanChange) (MediaTrack *trackid, int recvidx, double pan,
                                 bool relative);

void (*CSurf_OnPlayRateChange) (double playrate);
void (*CSurf_OnTempoChange) (double bpm);

void (*CSurf_OnOscControlMessage) (const char *msg, const float *arg);

double (*Master_GetPlayRate) (ReaProject *);
double (*Master_NormalizePlayRate) (double playrate, bool isnormalized);
double (*Master_GetTempo) ();
double (*Master_NormalizeTempo) (double bpm, bool isnormalized);

void (*kbd_OnMidiEvent) (MIDI_event_t *evt, int dev_index);
void (*TrackList_UpdateAllExternalSurfaces) ();
int (*GetMasterMuteSoloFlags) ();
void (*ClearAllRecArmed) ();
void (*SetTrackAutomationMode) (MediaTrack *tr, int mode);
int (*GetTrackAutomationMode) (MediaTrack *tr);
bool (*AnyTrackSolo) (ReaProject *);
void (*SoloAllTracks) (int solo); // solo=2 for SIP
void (*MuteAllTracks) (bool mute);
void (*BypassFxAllTracks) (
    int bypass); // -1 = bypass all if not all bypassed, otherwise unbypass all
const char *(*GetTrackInfo) (INT_PTR track, int *flags);
void *(*GetSetMediaTrackInfo) (INT_PTR track, const char *parmname,
                               void *setNewValue);
int (*GetMasterTrackVisibility) ();
MediaTrack *(*SetMixerScroll) (MediaTrack *leftmosttrack);
MediaTrack *(*GetMixerScroll) ();

bool (*IsTrackSelected) (MediaTrack *tr);
void (*SetTrackSelected) (MediaTrack *tr, bool sel);
void (*SetOnlyTrackSelected) (MediaTrack *);
void (*UpdateTimeline) (void);
int (*GetPlayState) ();
double (*GetPlayPosition) ();
double (*GetCursorPosition) ();

int (*GetSetRepeat) (int val);
void (*GoToMarker) (ReaProject *, int, bool);
void (*GoToRegion) (ReaProject *, int, bool);
int (*EnumProjectMarkers3) (ReaProject *, int idx, bool *isrgn, double *pos,
                            double *rgnend, const char **name,
                            int *markrgnindexnumber, int *colorOut);
int (*CountProjectMarkers) (ReaProject *, int *, int *);
void (*GetLastMarkerAndCurRegion) (ReaProject *, double, int *, int *);

void (*format_timestr_pos) (double tpos, char *buf, int buflen,
                            int modeoverride); // modeoverride=-1 for proj
void (*SetAutomationMode) (int mode,
                           bool onlySel); // sets all or selected tracks
void (*Main_UpdateLoopInfo) (int ignoremask);
bool (*Loop_OnArrow) (ReaProject *, int dir);

double (*TimeMap2_timeToBeats) (void *proj, double tpos, int *measures,
                                int *cml, double *fullbeats, int *cdenom);
double (*Track_GetPeakInfo) (MediaTrack *tr, int chidx);
double (*Track_GetPeakHoldDB) (MediaTrack *tr, int chidx, bool clear);

bool (*SetProjectMarkerByIndex2) (ReaProject *__proj, int idx, bool isrgn,
                                  double pos, double rgnend, int ID,
                                  const char *name, int color, int flags);
int (*AddProjectMarker2) (ReaProject *__proj, bool isrgn, double pos,
                          double rgnend, const char *name, int wantidx,
                          int color);

void (*mkvolpanstr) (char *str, double vol, double pan);
void (*mkvolstr) (char *str, double vol);
void (*mkpanstr) (char *str, double pan);

bool (*GetTrackUIVolPan) (MediaTrack *tr, double *vol, double *pan);
bool (*GetTrackUIPan) (MediaTrack *tr, double *pan1, double *pan2, int *mode);

bool (*GetTrackSendUIVolPan) (MediaTrack *tr, int sendidx, double *vol,
                              double *pan);
bool (*GetTrackSendName) (MediaTrack *tr, int sendidx, char *buf, int buflen);
bool (*GetTrackReceiveUIVolPan) (MediaTrack *tr, int recvidx, double *vol,
                                 double *pan);
bool (*GetTrackReceiveName) (MediaTrack *tr, int recvidx, char *buf,
                             int buflen);

void (*Main_OnCommand) (int command, int flag);
void (*KBD_OnMainActionEx) (int cmd, int val, int valhw, int relmode, HWND hwnd,
                            ReaProject *__proj);
void (*kbd_pushPopSoftTakeover) (const char *, const void *);
int (*NamedCommandLookup) (const char *);
HWND (*GetMainHwnd) ();

int (*GetToggleCommandState) (int command);
bool (*MIDIEditor_LastFocused_OnCommand) (int cmd, bool islistview);

void (*MoveEditCursor) (double adjamt, bool dosel);
void (*adjustZoom) (
    double amt, int forceset, bool doupd,
    int centermode);        // forceset=0, doupd=true, centermode=-1 for default
double (*GetHZoomLevel) (); // returns pixels/second

int (*TrackFX_GetCount) (MediaTrack *tr);
int (*TrackFX_GetRecCount) (MediaTrack *tr);
int (*TrackFX_GetInstrument) (MediaTrack *tr);
int (*TrackFX_GetEQ) (MediaTrack *tr, bool instantiate);
int (*TrackFX_SetEQParam) (MediaTrack *tr, int fxidx, int bandtype, int bandidx,
                           int paramtype, double val, bool isnorm);
bool (*TrackFX_GetEQParam) (MediaTrack *tr, int fxidx, int paramidx,
                            int *bandtype, int *bandidx, int *parmtype,
                            double *normval);
bool (*TrackFX_SetEQBandEnabled) (MediaTrack *tr, int fxidx, int bandtype,
                                  int bandidx, bool enable);
bool (*TrackFX_GetEQBandEnabled) (MediaTrack *tr, int fxidx, int bandtype,
                                  int bandidx);
int (*TrackFX_GetNumParams) (MediaTrack *tr, int fx);
bool (*TrackFX_GetFXName) (MediaTrack *tr, int fx, char *buf, int buflen);
bool (*TrackFX_NavigatePresets) (MediaTrack *tr, int fx, int chg);
bool (*TrackFX_GetPreset) (MediaTrack *tr, int fx, char *buf, int buflen);
bool (*TrackFX_SetPreset) (MediaTrack *tr, int fx, const char *buf);
double (*TrackFX_GetParam) (MediaTrack *tr, int fx, int param, double *minval,
                            double *maxval);
double (*TrackFX_GetParamEx) (MediaTrack *tr, int fx, int param, double *minval,
                              double *maxval, double *midval);
bool (*TrackFX_SetParam) (MediaTrack *tr, int fx, int param, double val);
bool (*TrackFX_EndParamEdit) (MediaTrack *tr, int fx, int param);
bool (*TrackFX_GetParamName) (MediaTrack *tr, int fx, int param, char *buf,
                              int buflen);
bool (*TrackFX_GetFormattedParamValue) (MediaTrack *tr, int fx, int param,
                                        char *buf, int buflen);
int (*TrackFX_GetParamFromIdent) (MediaTrack *tr, int fx, const char *buf);

double (*TrackFX_GetParamNormalized) (MediaTrack *track, int fx, int param);
bool (*TrackFX_SetParamNormalized) (MediaTrack *track, int fx, int param,
                                    double value);

int (*TrackFX_GetChainVisible) (MediaTrack *);
HWND (*TrackFX_GetFloatingWindow) (MediaTrack *, int);
void (*TrackFX_Show) (MediaTrack *, int, int);
bool (*TrackFX_GetEnabled) (MediaTrack *, int);
void (*TrackFX_SetEnabled) (MediaTrack *, int, bool);
bool (*TrackFX_GetOpen) (MediaTrack *, int);
void (*TrackFX_SetOpen) (MediaTrack *, int, bool);

MediaTrack *(*GetLastTouchedTrack) ();
bool (*GetLastTouchedFX) (int *trackidx, int *fxidx, int *parmidx);
bool (*GetFocusedFX) (int *trackidx, int *itemidx, int *fxidx);

double (*TimeMap2_beatsToTime) (ReaProject *proj, double tpos, int *measures);
double (*parse_timestr_pos) (const char *buf, int mode);
void (*SetEditCurPos) (double time, bool moveview, bool seekplay);

GUID *(*GetTrackGUID) (MediaTrack *tr);

const char *(*GetOscCfgDir) ();

const char *(*get_ini_file) ();
const char *(*GetAppVersion) ();
const char *(*GetResourcePath) ();
int (*RecursiveCreateDirectory) (const char *, void *);
bool (*WDL_ChooseDirectory) (HWND parent, const char *text,
                             const char *initialdir, char *fn, int fnsize,
                             bool preservecwd);
char *(*WDL_ChooseFileForOpen) (HWND, const char *, const char *, const char *,
                                const char *, const char *, bool, bool,
                                const char *, void *,
#ifdef _WIN32
                                HINSTANCE hInstance
#else
                                struct SWELL_DialogResourceIndex *
#endif
);

int (*GetNumTracks) ();
void (*format_timestr) (double, char *, int);
void (*guidToString) (GUID *g, char *dest);
void (*Undo_OnStateChangeEx) (const char *descchange, int whichStates,
                              int trackparm);
void (*Undo_BeginBlock) ();
void (*Undo_EndBlock) (const char *, int);
void (*CSurf_FlushUndo) (bool force);
int (*Plugin_Register) (const char *name, void *infostruct);

bool (*ToggleTrackSendUIMute) (MediaTrack *tr, int sendidx);
bool (*GetTrackSendUIMute) (MediaTrack *tr, int sendidx, bool *mute);
bool (*GetTrackReceiveUIMute) (MediaTrack *tr, int sendidx, bool *mute);
bool (*SetTrackSendUIPan) (MediaTrack *tr, int sendidx, double pan, int isend);
bool (*SetTrackSendUIVol) (MediaTrack *tr, int sendidx, double vol, int isend);
void *(*GetSetTrackSendInfo) (MediaTrack *tr, int category, int sendidx,
                              const char *parmname, void *setNewValue);
int (*GetTrackNumSends) (MediaTrack *tr, int category);

MediaTrack *(*GetTrack) (ReaProject *proj, int tridx);
bool (*GetTrackMIDILyrics) (MediaTrack *tr, int flag, char *buf, int *buflen);

bool (*GetProjExtState) (ReaProject *proj, const char *extname, const char *key,
                         char *val, int vallen);
const char *(*GetExtState) (const char *section, const char *key);
void (*SetExtState) (const char *section, const char *key, const char *val,
                     bool persist);
int (*SetProjExtState) (ReaProject *proj, const char *extname, const char *key,
                        const char *val);
void (*GetSet_LoopTimeRange2) (ReaProject *__proj, bool isSet, bool isLoop,
                               double *start, double *end, bool allowautoseek);

int *g_config_csurf_rate;
int *g_config_zoommode;

int *g_vu_minvol;
int *g_vu_maxvol;
int *g_config_vudecay;

int __g_projectconfig_timemode2;
int __g_projectconfig_timemode;
int __g_projectconfig_measoffs;
int __g_projectconfig_timeoffs; // double
int __g_projectconfig_show_grid;
int __g_projectconfig_autoxfade;
int __g_projectconfig_metronome_en;

#include <chrono>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

inline void Logger (string logMsg) {
  time_t now = chrono::system_clock::to_time_t (chrono::system_clock::now ());
  stringstream filepath;
  filepath << "~/patchamama_reaper_" << put_time (localtime (&now), "%F")
           << ".txt";
  ofstream ofs (filepath.str ().c_str (), ios_base::out | ios_base::app);
  ofs << put_time (localtime (&now), "%H:%M:%S ") << logMsg << '\n';
  ofs.close ();
}

extern "C" {

REAPER_PLUGIN_DLL_EXPORT int
REAPER_PLUGIN_ENTRYPOINT (REAPER_PLUGIN_HINSTANCE hInstance,
                          reaper_plugin_info_t *rec) {
  g_hInst = hInstance;

  if (!rec || rec->caller_version != REAPER_PLUGIN_VERSION || !rec->GetFunc)
    return 0;

  g_hwnd = rec->hwnd_main;
  int errcnt = 0;
#define IMPAPI(x)                                                              \
  if (!((*((void **)&(x)) = (void *)rec->GetFunc (#x))))                       \
    errcnt++;

  IMPAPI (update_disk_counters)
  IMPAPI (DB2SLIDER)
  IMPAPI (SLIDER2DB)
  IMPAPI (GetNumMIDIInputs)
  IMPAPI (GetNumMIDIOutputs)
  IMPAPI (CreateMIDIInput)
  IMPAPI (CreateMIDIOutput)
  IMPAPI (GetMIDIOutputName)
  IMPAPI (GetMIDIInputName)
  IMPAPI (CSurf_TrackToID)
  IMPAPI (CSurf_TrackFromID)
  IMPAPI (CSurf_NumTracks)
  IMPAPI (CSurf_SetTrackListChange)
  IMPAPI (CSurf_SetSurfaceVolume)
  IMPAPI (CSurf_SetSurfacePan)
  IMPAPI (CSurf_SetSurfaceMute)
  IMPAPI (CSurf_SetSurfaceSelected)
  IMPAPI (CSurf_SetSurfaceSolo)
  IMPAPI (CSurf_SetSurfaceRecArm)
  IMPAPI (CSurf_GetTouchState)
  IMPAPI (CSurf_SetAutoMode)
  IMPAPI (CSurf_SetPlayState)
  IMPAPI (CSurf_SetRepeatState)
  IMPAPI (CSurf_OnVolumeChange)
  IMPAPI (CSurf_OnPanChange)
  IMPAPI (CSurf_OnWidthChange)
  IMPAPI (CSurf_OnVolumeChangeEx)
  IMPAPI (CSurf_OnPanChangeEx)
  IMPAPI (CSurf_OnWidthChangeEx)
  IMPAPI (CSurf_OnMuteChange)
  IMPAPI (CSurf_OnMuteChangeEx)
  IMPAPI (CSurf_OnSelectedChange)
  IMPAPI (CSurf_OnSoloChange)
  IMPAPI (CSurf_OnSoloChangeEx)
  IMPAPI (CSurf_OnFXChange)
  IMPAPI (CSurf_OnRecArmChange)
  IMPAPI (CSurf_OnRecArmChangeEx)
  IMPAPI (CSurf_OnInputMonitorChange)
  IMPAPI (CSurf_OnInputMonitorChangeEx)
  IMPAPI (CSurf_OnPlay)
  IMPAPI (CSurf_OnStop)
  IMPAPI (CSurf_OnPause)
  IMPAPI (CSurf_OnRewFwd)
  IMPAPI (CSurf_OnRecord)
  IMPAPI (CSurf_GoStart)
  IMPAPI (CSurf_GoEnd)
  IMPAPI (CSurf_OnArrow)
  IMPAPI (CSurf_OnScroll)
  IMPAPI (CSurf_OnZoom)
  IMPAPI (CSurf_OnTrackSelection)
  IMPAPI (CSurf_ResetAllCachedVolPanStates)
  IMPAPI (CSurf_ScrubAmt)
  IMPAPI (CSurf_OnSendVolumeChange)
  IMPAPI (CSurf_OnSendPanChange)
  IMPAPI (CSurf_OnRecvVolumeChange)
  IMPAPI (CSurf_OnRecvPanChange)
  IMPAPI (CSurf_OnPlayRateChange)
  IMPAPI (CSurf_OnTempoChange)
  IMPAPI (CSurf_OnOscControlMessage)

  IMPAPI (Master_GetPlayRate)
  IMPAPI (Master_NormalizePlayRate)
  IMPAPI (Master_GetTempo)
  IMPAPI (Master_NormalizeTempo)

  IMPAPI (VkbStuffMessage)

  IMPAPI (TrackList_UpdateAllExternalSurfaces)
  IMPAPI (kbd_OnMidiEvent)
  IMPAPI (GetMasterMuteSoloFlags)
  IMPAPI (ClearAllRecArmed)
  IMPAPI (SetTrackAutomationMode)
  IMPAPI (GetTrackAutomationMode)
  IMPAPI (AnyTrackSolo)
  IMPAPI (SoloAllTracks)
  IMPAPI (MuteAllTracks)
  IMPAPI (BypassFxAllTracks)
  IMPAPI (GetTrackInfo)
  IMPAPI (GetSetMediaTrackInfo)
  IMPAPI (GetMasterTrackVisibility)
  IMPAPI (SetMixerScroll)
  IMPAPI (GetMixerScroll)
  IMPAPI (IsTrackSelected)
  IMPAPI (SetTrackSelected)
  IMPAPI (SetOnlyTrackSelected)
  IMPAPI (SetAutomationMode)
  IMPAPI (UpdateTimeline)
  IMPAPI (Main_UpdateLoopInfo)
  IMPAPI (Loop_OnArrow)
  IMPAPI (GetPlayState)
  IMPAPI (GetPlayPosition)
  IMPAPI (GetCursorPosition)
  IMPAPI (format_timestr_pos)
  IMPAPI (TimeMap2_timeToBeats)
  IMPAPI (Track_GetPeakInfo)
  IMPAPI (Track_GetPeakHoldDB)
  IMPAPI (SetProjectMarkerByIndex2)
  IMPAPI (AddProjectMarker2)
  IMPAPI (GetTrackUIVolPan)
  IMPAPI (GetTrackUIPan)
  IMPAPI (GetTrackSendUIVolPan)
  IMPAPI (GetTrackSendName)
  IMPAPI (GetTrackReceiveUIVolPan)
  IMPAPI (GetTrackReceiveName)
  IMPAPI (GetSetRepeat)
  IMPAPI (GoToMarker)
  IMPAPI (GoToRegion)
  IMPAPI (EnumProjectMarkers3)
  IMPAPI (CountProjectMarkers)
  IMPAPI (GetLastMarkerAndCurRegion)
  IMPAPI (mkvolpanstr)
  IMPAPI (mkvolstr)
  IMPAPI (mkpanstr)
  IMPAPI (MoveEditCursor)
  IMPAPI (adjustZoom)
  IMPAPI (GetHZoomLevel)
  IMPAPI (Main_OnCommand)
  IMPAPI (KBD_OnMainActionEx)
  IMPAPI (kbd_pushPopSoftTakeover)
  IMPAPI (NamedCommandLookup)
  IMPAPI (GetMainHwnd)
  IMPAPI (GetToggleCommandState)
  IMPAPI (MIDIEditor_LastFocused_OnCommand)

  IMPAPI (TrackFX_GetCount)
  IMPAPI (TrackFX_GetRecCount)
  IMPAPI (TrackFX_GetInstrument)
  IMPAPI (TrackFX_GetEQ)
  IMPAPI (TrackFX_SetEQParam)
  IMPAPI (TrackFX_GetEQParam)
  IMPAPI (TrackFX_SetEQBandEnabled)
  IMPAPI (TrackFX_GetEQBandEnabled)

  IMPAPI (TrackFX_GetNumParams)
  IMPAPI (TrackFX_GetParam)
  IMPAPI (TrackFX_GetParamEx)
  IMPAPI (TrackFX_SetParam)
  IMPAPI (TrackFX_EndParamEdit)
  IMPAPI (TrackFX_GetParamName)
  IMPAPI (TrackFX_GetFormattedParamValue)
  IMPAPI (TrackFX_GetParamFromIdent)

  IMPAPI (TrackFX_GetParamNormalized)
  IMPAPI (TrackFX_SetParamNormalized)

  IMPAPI (TrackFX_GetFXName)
  IMPAPI (TrackFX_NavigatePresets)
  IMPAPI (TrackFX_GetPreset)
  IMPAPI (TrackFX_SetPreset)

  IMPAPI (TrackFX_GetChainVisible)
  IMPAPI (TrackFX_GetFloatingWindow)
  IMPAPI (TrackFX_Show)
  IMPAPI (TrackFX_GetEnabled)
  IMPAPI (TrackFX_SetEnabled)
  IMPAPI (TrackFX_GetOpen)
  IMPAPI (TrackFX_SetOpen)

  IMPAPI (GetLastTouchedTrack)
  IMPAPI (GetLastTouchedFX)
  IMPAPI (GetFocusedFX)

  IMPAPI (TimeMap2_beatsToTime)
  IMPAPI (parse_timestr_pos)
  IMPAPI (SetEditCurPos)

  IMPAPI (GetTrackGUID)
  IMPAPI (GetOscCfgDir)
  IMPAPI (get_ini_file)
  IMPAPI (GetResourcePath)
  IMPAPI (GetAppVersion)
  IMPAPI (RecursiveCreateDirectory)
  IMPAPI (WDL_ChooseFileForOpen)
  IMPAPI (WDL_ChooseDirectory)

  IMPAPI (GetNumTracks)
  IMPAPI (format_timestr)
  IMPAPI (guidToString)
  IMPAPI (Undo_OnStateChangeEx)
  IMPAPI (Undo_BeginBlock)
  IMPAPI (Undo_EndBlock)
  IMPAPI (CSurf_FlushUndo)
  IMPAPI (ToggleTrackSendUIMute)
  IMPAPI (GetTrackSendUIMute)
  IMPAPI (GetTrackReceiveUIMute)
  IMPAPI (SetTrackSendUIPan)
  IMPAPI (SetTrackSendUIVol)
  IMPAPI (GetSetTrackSendInfo)
  IMPAPI (GetTrackNumSends)

  IMPAPI (GetTrack)
  IMPAPI (GetTrackMIDILyrics)
  IMPAPI (GetProjExtState)
  IMPAPI (GetExtState)
  IMPAPI (SetProjExtState)
  IMPAPI (SetExtState)
  IMPAPI (GetSet_LoopTimeRange2)

  void *(*get_config_var) (const char *name, int *szout);
  int (*projectconfig_var_getoffs) (const char *name, int *szout);
  IMPAPI (get_config_var);
  IMPAPI (projectconfig_var_getoffs);
  IMPAPI (projectconfig_var_addr);
  if (errcnt)
    return 0;

  return 1;
}
}; // extern "C"
