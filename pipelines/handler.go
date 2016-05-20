package pipelines

import (
	"database/sql"
	"net/http"

	"github.com/concourse/atc/db"
)

type PipelineHandlerFactory struct {
	pipelineDBFactory db.PipelineDBFactory
	teamDBFactory     db.TeamDBFactory
}

func NewHandlerFactory(
	pipelineDBFactory db.PipelineDBFactory,
	teamDBFactory db.TeamDBFactory,
) *PipelineHandlerFactory {
	return &PipelineHandlerFactory{
		pipelineDBFactory: pipelineDBFactory,
		teamDBFactory:     teamDBFactory,
	}
}

func (pdbh *PipelineHandlerFactory) HandlerFor(pipelineScopedHandler func(db.PipelineDB) http.Handler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		pipelineName := r.FormValue(":pipeline_name")
		teamName := r.FormValue(":team_name")
		teamDB := pdbh.teamDBFactory.GetTeamDB(teamName)
		savedPipeline, err := teamDB.GetPipelineByName(pipelineName)
		if err != nil {
			if err == sql.ErrNoRows {
				w.WriteHeader(http.StatusNotFound)
			} else {
				w.WriteHeader(http.StatusInternalServerError)
			}
			return
		}
		pipelineDB := pdbh.pipelineDBFactory.Build(savedPipeline)

		pipelineScopedHandler(pipelineDB).ServeHTTP(w, r)
	}
}
